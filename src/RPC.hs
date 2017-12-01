{-|

Remote procedure call interface for interacting with an individual node.

-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module RPC (
  -- ** Server
  rpcServer,

  -- ** Types
  RPCCmd(..),
  RPCResponse(..),
) where

import Protolude hiding (sourceLine, sourceColumn, catch)

import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Exception.Lifted (catch)

import DB
import NodeState (NodeT, runNodeT)
import Script.Pretty (prettyPrint)
import qualified DB
import qualified Key
import qualified Utils
import qualified Asset
import qualified Block
import qualified Config
import qualified Script
import qualified Ledger
import qualified Account
import qualified Version
import qualified MemPool
import qualified Address
import qualified Contract
import qualified Validate
import qualified NodeState
import qualified Derivation
import qualified Transaction as Tx
import qualified Script.Pretty as Pretty
import qualified Script.Parser as Parser
import qualified Network.P2P.Cmd as Cmd

import Data.Aeson (ToJSON(..), FromJSON, Value(..), (.=), (.:), (.:?), object)
import Data.Aeson.Types (typeMismatch)
import qualified Data.Aeson as A
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.Lazy as TL

import Control.Distributed.Process (nodeAddress, processNodeId)
import Network.Transport (endPointAddressToByteString)

import qualified Control.Concurrent.Chan as Chan

import Network
import Network.HTTP.Client
import Network.HTTP.Types.Status
import qualified Web.Scotty.Trans as WS
import qualified Web.Scotty.Internal.Types as WS
import qualified Network.Wai as Warp
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp
import qualified Network.Wai.Logger as WL

instance WS.ScottyError Text where
  stringError = (toS :: TL.Text -> Text) . WS.stringError
  showError = WS.showError . (toSL :: Text -> TL.Text)

-------------------------------------------------------
-- RPC Server
-------------------------------------------------------

opts :: WL.ApacheLogger -> WS.Options
opts logger = WS.Options
  { verbose = 0
  , settings =
      Warp.setHTTP2Disabled
    $ Warp.setServerName "Uplink/0.1 (Unix)"
    $ Warp.setLogger logger
    $ Warp.setTimeout 20 Warp.defaultSettings
  }

scottyTLS
  :: MonadIOReadDB m
  => WL.ApacheLogger
  -> Warp.Port
  -> FilePath         -- ^ SSL Key
  -> FilePath         -- ^ SSL Certificate
  -> (NodeT m Warp.Response -> IO Warp.Response)
  -> (WS.ScottyT Text (NodeT m) () -> IO ())
scottyTLS logger port key cert runDBInIO =
    Warp.runTLS tlsSettings warpSettings <=<
      WS.scottyAppT runDBInIO
  where
    tlsSettings = Warp.defaultTlsSettings
      { Warp.keyFile = key
      , Warp.certFile = cert
      }

    warpSettings = Warp.setLogger logger $
      Warp.setPort port Warp.defaultSettings

rpcServer
  :: MonadIOReadDB m
  => Config.Config
  -> Chan.Chan Cmd.Cmd
  -> (NodeT m Warp.Response -> IO Warp.Response)
  -> IO ()
rpcServer config chan runInIO = do

    let rpcPort = Config.rpcPort config
    let key     = Config.rpcKey config
    let cert    = Config.rpcCrt config

    -- XXX Move from Stdout to a log file (set in Config?)
    WL.withStdoutLogger $ \appLogger ->

      -- Die with better error message if server can't be started. This is
      -- *probably* and ok catch-all, because exceptions during execution of RPC
      -- server are handled within `rpcApi`. This *should* only fail for one
      -- reason: the socket the RPC server binds to is already in use.
      catchFailedSocketBind rpcPort $ do
        -- Setup HTTP server for RPC
        if Config.rpcSsl config
          then
            scottyTLS appLogger rpcPort key cert runInIO $
              rpcApi chan
          else do
            sock <- listenOn $ PortNumber (fromIntegral rpcPort)
            WS.scottySocketT (opts appLogger) sock runInIO $
              rpcApi chan
  where
    catchFailedSocketBind port a =
      catch a $ \(SomeException err) ->
        Utils.dieRed $ mconcat
          [ "Could not start RPC server on port "
          , show port, ":\n    "
          , show err
          ]

-------------------------------------------------------------------------------
-- API Routes
-------------------------------------------------------------------------------

type MonadIOReadDB m = (MonadBaseControl IO m, MonadIO m, MonadReadDB m)
type RpcT m a = WS.ScottyT Text (NodeT m) a

-- Helper `WS.post` function that catches all fatal errors
-- thrown by `panic` in the `ScottyT e (NodeT m) a` stack
post_
  :: MonadIOReadDB m
  => WS.RoutePattern
  -> WS.ActionT Text (NodeT m) ()
  -> RpcT m ()
post_ route action =
  WS.post route $ do
    isTestNode <- lift $ NodeState.isTestNode
    catch action $ \(FatalError e) ->
      if isTestNode
        then jsonInternalErr $ show e
        else jsonInternalErr "Uplink encountered a fatal internal error."

rpcApi
  :: (MonadIO m, MonadBaseControl IO m, MonadReadDB m)
  => Chan.Chan Cmd.Cmd
  -> RpcT m ()
rpcApi chan = do

    --------------------------------------------------
    -- RPC Command API
    --------------------------------------------------

    post_ "/" $ do
      config <- lift $ NodeState.askConfig
      if Config.rpcReadOnly config
        then jsonReadOnlyErr
        else do
          reqBody <- WS.body
          case A.eitherDecode reqBody of
            Left err -> jsonParseErr $ toS err
            Right val -> do
              resp <- lift $ handleRPCCmd chan val
              WS.json $ toJSON resp

    --------------------------------------------------
    -- RPC Query API
    --------------------------------------------------

    ---------------------------
    -- Status API
    ---------------------------

    post_ "/health" $
      WS.status status200

    post_ "/version" $
      WS.json $ object [
          "version" .= Version.version
        , "branch"  .= Version.branch
        , "commit"  .= Version.commit
        , "dirty"   .= Version.dirty
        ]
    ---------------------------
    -- Blocks API
    ---------------------------

    post_ "/blocks" $ do
      eBlocks <- lift $ lift DB.readBlocks
      case eBlocks of
        Left err -> jsonInternalErr $ toS err
        Right blocks -> jsonRPCRespM blocks

    post_ "/blocks/:blockIdx" $ do
      eBlockIdx <- WS.parseParam <$> WS.param "blockIdx"
      case eBlockIdx of
        Left _ -> do
          let errMsg = "Block index should be a positive integer."
          jsonInvalidParamErr errMsg
        Right blockIdx -> do
          eBlock <- lift $
            lift $ DB.readBlock blockIdx
          case eBlock of
            Left err -> jsonNotFoundErr $ toS err
            Right block -> jsonRPCRespM block

    ---------------------------
    -- Peers API
    ---------------------------

    post_ "/peers" $ do
      peers <- lift NodeState.getPeers
      jsonRPCRespM peers

    post_ "/peers/validators" $ do
      validatorPeers <- lift NodeState.getValidatorPeers
      jsonRPCRespM validatorPeers

    ---------------------------
    -- Accounts API
    ---------------------------

    post_ "/accounts" $
      jsonRPCRespM =<< lift getAccounts

    post_ "/accounts/:addr" $ do
      addr <- Address.fromRaw <$> WS.param "addr"
      if Address.validateAddress addr
        then do
          eAcct <- lift $ getAccount addr
          either (jsonNotFoundErr . toS) jsonRPCRespM eAcct
      else jsonInvalidParamErr "Address supplied is an invalid Sha256 hash"

    ---------------------------
    -- Assets API
    ---------------------------

    post_ "/assets" $ do
      assetsWithAddrs <- lift getAssetsWithAddrs
      jsonRPCRespM $ map (uncurry $ Asset.assetWithAddrJSON) assetsWithAddrs

    post_ "/assets/:addr" $ do
      addr <- Address.fromRaw <$> WS.param "addr"
      if Address.validateAddress addr
        then do
          eAsset <- lift $ getAsset addr
          either (jsonNotFoundErr . toS) jsonRPCRespM eAsset
      else jsonInvalidParamErr "Address supplied is an invalid Sha256 hash"

    ---------------------------
    -- Contract API
    ---------------------------

    post_ "/contracts" $
      jsonRPCRespM =<< lift getContracts

    post_ "/contracts/:addr" $ do
      addr <- Address.fromRaw <$> WS.param "addr"
      if Address.validateAddress addr
        then do
          eContract <- lift $ getContract addr
          either (jsonNotFoundErr . toS) jsonRPCRespM eContract
      else jsonInvalidParamErr "Address supplied is an invalid Sha256 hash"

    post_ "/contracts/:addr/callable" $ do
      addr <- Address.fromRaw <$> WS.param "addr"
      if Address.validateAddress addr
        then do
          eContract <- lift $ getContract addr
          case eContract of
            Left err -> jsonNotFoundErr $ toS err
            Right contract -> do
              let methods = Contract.callableMethods contract
              let methodsArgsAndTypes = flip map methods $ \m ->
                    (Script.methodName m, Script.argtys m)
              jsonRPCRespM $ jsonMethodsArgsAndTypes methodsArgsAndTypes
      else jsonInvalidParamErr "Address supplied is an invalid Sha256 hash"

    ---------------------------
    -- Transactions API
    ---------------------------

    post_ "/txlog" $ do
      jsonRPCRespM ()

    post_ "/transactions/pool" $ do
      memPool <- lift NodeState.getTxMemPool
      jsonRPCRespM memPool

    post_ "/transactions/pool/size" $ do
      memPool <- lift NodeState.getTxMemPool
      jsonRPCRespM $ MemPool.size memPool

    post_ "/transactions/pool/all" $ do
      memPoolMap <- lift getAllMemPools
      jsonRPCRespM (memPoolMap :: Map Text (Either Text MemPool.MemPool))

    post_ "/transactions/pool/all/sizes" $ do
      memPoolsSizeMap <- lift getAllMemPoolSizes
      jsonRPCRespM (memPoolsSizeMap :: Map Text (Either Text Int))

    -- Query the status of a transaction
    post_ "/transactions/status/:hash" $ do
      eTxHash <- WS.parseParam <$> WS.param "hash"
      case encodeUtf8 <$> eTxHash of
        Left _ -> jsonInvalidParamErr "Invalid transaction hash"
        Right txHash -> jsonRPCRespM =<<
          lift (NodeState.getTxStatus txHash)

    -- XXX Pagination, because could have unbounded invalid txs
    post_ "/transactions/invalid" $ do
      eRes <- lift $ lift DB.readInvalidTxs
      case eRes of
        Left err -> jsonInternalErr $ show err
        Right res -> jsonRPCRespM res

    -- Query a specific invalid tx from DB
    -- XXX look in invalidTxPool (in memory) before DB
    post_ "/transactions/invalid/:hash" $ do
      eTxHash <- WS.parseParam <$> WS.param "hash"
      case encodeUtf8 <$> eTxHash of
        Left _ -> jsonInvalidParamErr "Invalid transaction hash"
        Right txHash -> do
          itx <- lift $ lift $ getInvalidTx txHash
          jsonRPCRespM itx

    post_ "/transactions/:blockIdx" $ do
      eBlockIdx <- WS.parseParam <$> WS.param "blockIdx"
      case eBlockIdx of
        Left _ -> do
          let errMsg = "Block index should be a positive integer."
          jsonInvalidParamErr errMsg
        Right blockIdx -> do
          eTxs <- lift $ lift $ getTransactions blockIdx
          either (jsonNotFoundErr . toS) jsonRPCRespM eTxs

    post_ "/transactions/:blockIdx/:txIdx" $ do
      eBlockIdx <- WS.parseParam <$> WS.param "blockIdx"
      eTxIdx    <- WS.parseParam <$> WS.param "txIdx"
      case (,) <$> eBlockIdx <*> eTxIdx of
        Left _ -> do
          let errMsg = "Both block and transaction indexs should be positive integers."
          jsonInvalidParamErr errMsg
        Right (blockIdx, txIdx) -> do
          eTx <- lift $ lift $ getTransaction blockIdx txIdx
          either (jsonNotFoundErr . toS) jsonRPCRespM eTx


-- | Construct a RPC response from a serializeable structure
jsonRPCRespM :: (Monad m, ToJSON a) => a -> WS.ActionT Text m ()
jsonRPCRespM = WS.json . RPCResp . toJSON

-- | Construct a RPC response indicating success
jsonRPCRespOK :: Monad m => WS.ActionT Text m ()
jsonRPCRespOK = WS.json $ RPCRespOK

jsonInvalidParamErr :: Monad m => Text -> WS.ActionT Text m ()
jsonInvalidParamErr = WS.json . RPCRespError . InvalidParam

jsonInternalErr :: Monad m => Text -> WS.ActionT Text m ()
jsonInternalErr = WS.json . RPCRespError . Internal

jsonParseErr :: Monad m => Text -> WS.ActionT Text m ()
jsonParseErr = WS.json . RPCRespError . JSONParse

jsonReadOnlyErr :: Monad m => WS.ActionT Text m ()
jsonReadOnlyErr = WS.json readOnlyErr

jsonNotFoundErr :: Monad m => Text -> WS.ActionT Text m ()
jsonNotFoundErr = WS.json . RPCRespError . NotFound

-------------------------------------------------------------------------------
-- Querying (Local NodeState or DB)
-------------------------------------------------------------------------------

getAccounts :: MonadBase IO m => NodeT m [Account.Account]
getAccounts = NodeState.withLedgerState $ pure . Map.elems . Ledger.accounts

getAccount :: MonadBase IO m => Address.Address -> NodeT m (Either Text Account.Account)
getAccount addr = NodeState.withLedgerState $ pure . first show . Ledger.lookupAccount addr

getAssetsWithAddrs :: MonadBase IO m => NodeT m [(Address.Address, Asset.Asset)]
getAssetsWithAddrs = NodeState.withLedgerState $ pure . Map.toList . Ledger.assets

getAsset :: MonadBase IO m => Address.Address -> NodeT m (Either Text Asset.Asset)
getAsset addr = NodeState.withLedgerState $ pure . first show . Ledger.lookupAsset addr

getContracts :: MonadBase IO m => NodeT m [Contract.Contract]
getContracts = NodeState.withLedgerState $ pure . Map.elems . Ledger.contracts

getContract :: MonadBase IO m => Address.Address -> NodeT m (Either Text Contract.Contract)
getContract addr = NodeState.withLedgerState $ pure . first show . Ledger.lookupContract addr

getTransactions :: MonadReadDB m => Int -> m (Either Text [Tx.Transaction])
getTransactions blockIdx = do
  eBlock <- DB.readBlock blockIdx
  return $ Block.transactions <$> eBlock

getTransaction :: MonadReadDB m => Int -> Int -> m (Either Text Tx.Transaction)
getTransaction blockIdx txIdx = do
  eTxs <- getTransactions blockIdx
  return $ case eTxs of
    Left err -> Left err
    Right [] -> Left "No transactions in the specified block"
    Right txs -> case txs `atMay` txIdx of
      Nothing -> Left $ toS $ "No transaction at index " ++ show txIdx
      Just tx -> Right tx

getInvalidTx :: MonadReadDB m => ByteString -> m (Either Text Tx.InvalidTransaction)
getInvalidTx = DB.readInvalidTx

getInvalidTxs :: MonadReadDB m => m (Either Text [Tx.InvalidTransaction])
getInvalidTxs = first show <$> DB.readInvalidTxs

-------------------------------------------------------------------------------
-- Querying (Network, using RPC)
-------------------------------------------------------------------------------

getAllMemPoolSizes :: MonadBase IO m => NodeT m (Map Text (Either Text Int))
getAllMemPoolSizes = queryAllRPC mkPeerUrl
  where
    urlPrefix = "POST http://"
    urlSuffix = ":8545/transactions/pool/size" -- XXX 8545 not guarenteed to be RPC port
    mkPeerUrl hn = urlPrefix <> toS hn <> urlSuffix

getAllMemPools :: MonadBase IO m => NodeT m (Map Text (Either Text MemPool.MemPool))
getAllMemPools = queryAllRPC mkPeerUrl
  where
    urlPrefix = "POST http://"
    urlSuffix = ":8545/transactions/pool" -- XXX 8545 not guarenteed to be RPC port
    mkPeerUrl hn = urlPrefix <> toS hn <> urlSuffix

queryAllRPC
  :: forall a m. (FromJSON a, MonadBase IO m)
  => (Text -> Text)
  -> NodeT m (Map Text (Either Text a))
queryAllRPC mkUrl = do
    manager   <- liftBase $ newManager defaultManagerSettings
    peerAddrs <- getPeerAddrs
    requests  <- liftBase $ mapM (parseRequest . toS . mkUrl) peerAddrs
    responseBodys <- liftBase $ mapM (fmap responseBody . flip httpLbs manager) requests
    let rpcResps = mapMaybe A.decode responseBodys
    let respVals = map (decodeVal . contents) rpcResps
    return $ Map.fromList $ zip peerAddrs respVals
  where
    nodeIdToHostname' = toS . NodeState.nodeIdToHostname
    getPeerAddrs = map nodeIdToHostname' <$> NodeState.getPeerNodeIds

    decodeVal :: A.Value -> Either Text a
    decodeVal v = case A.fromJSON v of
      A.Error err  -> Left $ toS err
      A.Success v' -> Right v'

-------------------------------------------------------------------------------
-- Command
-------------------------------------------------------------------------------

handleRPCCmd :: MonadBase IO m => Chan Cmd.Cmd -> RPCCmd -> NodeT m RPCResponse
handleRPCCmd chan rpcCmd = do
  (mP2PCmd, rpcResp) <- case rpcCmd of

    Transaction tx -> do
      liftBase $ putText $
        "RPC Recieved Transaction:\n\t" <> show tx

      validationRes <- NodeState.withLedgerState $ \worldState ->
        return $ first show $ do
          Validate.validateTransactionOrigin worldState tx
          Validate.verifyTransaction worldState tx

      let p2pCmd = Cmd.Transaction tx
      case validationRes of
        Left err -> return (Nothing, txErr err)
        Right _ -> return (Just p2pCmd, RPCRespOK)

    Test testCmd -> do
      isTestNode <- NodeState.isTestNode
      if not isTestNode then
        return (Nothing, notTestNodeErr)
      else handleTestRPCCmd testCmd

  case mP2PCmd of
    Nothing -> return ()
    Just p2pCmd -> liftBase $ writeChan chan p2pCmd

  return rpcResp

handleTestRPCCmd :: MonadBase IO m => TestRPCCmd -> NodeT m (Maybe Cmd.Cmd, RPCResponse)
handleTestRPCCmd testRPCCmd = do
  let p2pCmd = case testRPCCmd of

        SaturateNetwork nTxs nSecs ->
          Cmd.Test $ Cmd.SaturateNetwork nTxs nSecs

        ResetMemPools ->
          Cmd.Test $ Cmd.ResetMemPools

        ResetDB addr sig ->
          Cmd.Test $ Cmd.ResetDB addr sig

  return (Just p2pCmd, RPCRespOK)

-------------------------------------------------------------------------------
-- Protocol
-------------------------------------------------------------------------------

-- | An RPC Command (changes state of server)
data RPCCmd
  = Transaction Tx.Transaction
  | Test TestRPCCmd
  deriving (Eq, Show, Generic)

data TestRPCCmd
  = SaturateNetwork
    { nTxs  :: Int
    , nSecs :: Int
    }
  | ResetMemPools
  | ResetDB
    { address   :: Address.Address
    , signature :: ByteString
    }
  deriving (Eq, Show)

-- | An RPC response body
data RPCResponse
  = RPCResp { contents :: A.Value }
  | RPCRespError RPCResponseError
  | RPCRespOK
  deriving (Eq, Show, Generic, A.ToJSON, A.FromJSON)

-- | An RPC response error
data RPCResponseError
  = InvalidParam Text
  | Internal Text
  | ContractParse Parser.ParseErrInfo
  | ContractType Text
  | ContractGet Text
  | JSONParse Text
  | ReadOnly
  | Tx Text
  | NotTestNode
  | NotFound Text
  deriving (Eq, Show, Generic, A.FromJSON)

invalidParam :: Text -> RPCResponse
invalidParam = RPCRespError . InvalidParam

contractParseErr :: Parser.ParseErrInfo -> RPCResponse
contractParseErr = RPCRespError . ContractParse

contractTypeErr :: Text -> RPCResponse
contractTypeErr = RPCRespError . ContractType

readOnlyErr :: RPCResponse
readOnlyErr = RPCRespError ReadOnly

txErr :: Text -> RPCResponse
txErr = RPCRespError . Tx

contractErr :: Text -> RPCResponse
contractErr = RPCRespError . ContractGet

notFoundErr :: Text -> RPCResponse
notFoundErr = RPCRespError . NotFound

notTestNodeErr :: RPCResponse
notTestNodeErr = RPCRespError NotTestNode

jsonRPCResp :: ToJSON a => a -> RPCResponse
jsonRPCResp = RPCResp . toJSON

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

instance FromJSON RPCCmd where
  parseJSON (Object v) = do
    method <- v .: "method"
    params <- v .: "params"
    case method :: Text of

      "Transaction" -> Transaction <$> v .: "params"

      "Test" -> Test <$> A.parseJSON (A.Object params)

      invalid -> typeMismatch "RPCCmd" $ A.String invalid

  parseJSON invalid = typeMismatch "RPCCmd" invalid

instance FromJSON TestRPCCmd where
  parseJSON (Object v) = do
    method <- v .: "method"
    params <- v .: "params"
    case method :: Text of
      "SaturateNetwork" -> SaturateNetwork
        <$> params .: "nTxs"
        <*> params .: "nSecs"
      "ResetMemPools" -> pure ResetMemPools
      "ResetDB" -> ResetDB
        <$> params .: "address"
        <*> fmap encodeUtf8 (params .: "signature")
      invalid -> typeMismatch "TestRPCCmd" $ A.String invalid
  parseJSON invalid = typeMismatch "TestRPCCmd" invalid

mapTextToMapBS :: Map Text Text -> Map ByteString ByteString
mapTextToMapBS = Map.mapKeys encodeUtf8 . Map.map encodeUtf8

instance ToJSON RPCResponseError where
  toJSON (InvalidParam msg) = object
    [ "errorType" .= ("InvalidParam" :: Text)
    , "errorMsg"  .= msg
    ]
  toJSON (Internal msg) = object
    [ "errorType" .= ("Internal" :: Text)
    , "errorMsg"  .= msg
    ]
  toJSON (ContractParse parseErr) = object
    [ "errorType"      .= ("ContractParse" :: Text)
    , "errorMsg"       .= Parser.errMsg parseErr
    , "errorPosition"  .= object [
          "line"   .= Parser.line parseErr
        , "column" .= Parser.column parseErr
        ]
    ]
  toJSON (ContractType msg) = object
    [ "errorType" .= ("ContractType" :: Text)
    , "errorMsg"  .= msg
    ]
  toJSON (JSONParse msg) = object
    [ "errorType" .= ("JSONParse" :: Text)
    , "errorMsg"  .= msg
    ]
  toJSON ReadOnly = object
    [ "errorType" .= ("ReadOnly" :: Text)
    , "errorMsg"  .= ("Node is in read-only mode." :: Text)
    ]
  toJSON (Tx msg) = object
    [ "errorType" .= ("Tx" :: Text)
    , "errorMsg"  .= msg
    ]
  toJSON (ContractGet msg) = object
    [ "errorType" .= ("ContractGet" :: Text)
    , "errorMsg"  .= msg
    ]
  toJSON NotTestNode = object
    [ "errorType" .= ("NotTestNode" :: Text)
    , "errorMsg"  .= ("The node is not operating in 'test' mode." :: Text)
    ]
  toJSON (NotFound msg) = object
    [ "errorType" .= ("NotFound" :: Text)
    , "errorMsg"  .= ("Not Found: " <> msg :: Text)
    ]

-- | Deserialize RPC command (stateful)
decodeRpc :: LByteString -> Either [Char] RPCCmd
decodeRpc = A.eitherDecode

-- | Serialize RPC response
encodeRpc :: RPCResponse -> LByteString
encodeRpc = A.encode

-- | Encode an Address and Contract as a JSON object
contractWithAddrJSON :: Contract.Contract -> A.Value
contractWithAddrJSON contract = object
    [ "address"  .= decodeUtf8 (Address.rawAddr addr)
    , "contract" .= contract
    ]
  where
    addr = Derivation.addrContract contract

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

jsonMethodsArgsAndTypes
  :: [(Script.Name, [(Script.Name, Script.Type)])]
  -> Map Text [(Text,Text)]
jsonMethodsArgsAndTypes methodsArgsAndTypes =
  Map.fromList $ flip map methodsArgsAndTypes $ \(nm,ats) ->
    (prettyPrint nm, map (bimap prettyPrint prettyPrint) ats)
