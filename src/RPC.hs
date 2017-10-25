{-|

Remote procedure call interface for interacting with an individual node.

-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RPC (
  -- ** Server
  rpcServer,

  -- ** Types
  RPCCmd(..),
  RPCResponse(..),
) where

import Protolude hiding (sourceLine, sourceColumn, catch)

import Control.Monad.Base (MonadBase, liftBase)
import Control.Exception.Lifted (catch)

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
import qualified Transaction
import qualified Script.Pretty as Pretty
import qualified Script.Parser as Parser
import qualified Network.P2P.Cmd as Cmd

import Data.Aeson (ToJSON(..), FromJSON, Value(..), (.=), (.:), (.:?), object)
import Data.Aeson.Types (typeMismatch)
import qualified Data.Aeson as A
import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Distributed.Process (nodeAddress, processNodeId)
import Network.Transport (endPointAddressToByteString)

import qualified Control.Concurrent.Chan as Chan

import Network
import Network.HTTP.Client
import Network.HTTP.Types.Status
import qualified Web.Scotty as WS
import qualified Web.Scotty.Internal.Types as WS
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp
import qualified Network.Wai.Logger as WL

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
  :: WL.ApacheLogger
  -> Warp.Port
  -> FilePath         -- ^ SSL Key
  -> FilePath         -- ^ SSL Certificate
  -> (WS.ScottyM () -> IO ())
scottyTLS logger port key cert =
    Warp.runTLS tlsSettings warpSettings <=< WS.scottyApp
  where
    tlsSettings = Warp.defaultTlsSettings
      { Warp.keyFile = key
      , Warp.certFile = cert
      }

    warpSettings = Warp.setLogger logger $
      Warp.setPort port Warp.defaultSettings

rpcServer
  :: Config.Config
  -> Chan.Chan Cmd.Cmd
  -> NodeState.NodeConfig
  -> NodeState.NodeState
  -> IO ()
rpcServer config chan nodeConfig nodeState = do

    let rpcPort = Config.rpcPort $ NodeState.config nodeConfig
    let key  = Config.rpcKey config
    let cert = Config.rpcCrt config

    -- XXX Move from Stdout to a log file (set in Config?)
    WL.withStdoutLogger $ \apLogger ->

      -- Die with better error message if server can't be started. This is
      -- *probably* and ok catch-all, because exceptions during execution of RPC
      -- server are handled within `rpcApi`. This *should* only fail for one
      -- reason: the socket the RPC server binds to is already in use.
      catchFailedSocketBind rpcPort $ do
        -- Setup HTTP server for RPC
        if Config.rpcSsl config
          then scottyTLS apLogger rpcPort key cert (rpcApi chan nodeConfig nodeState)
          else do
            sock <- listenOn $ PortNumber (fromIntegral rpcPort)
            WS.scottySocket (opts apLogger) sock (rpcApi chan nodeConfig nodeState)
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

rpcApi
  :: Chan.Chan Cmd.Cmd
  -> NodeState.NodeConfig
  -> NodeState.NodeState
  -> WS.ScottyM ()
rpcApi chan nodeConfig nodeState = do

    let runNodeT' = liftIO . runNodeT nodeConfig nodeState

    -- Helper `WS.post` function that catches all fatal errors thrown by `panic`
    let post route action =
          WS.post route $ do
            isTestNode <- runNodeT' $ lift NodeState.isTestNode
            catch action $ \(FatalError e) ->
              if isTestNode
                then jsonInternalErr $ show e
                else jsonInternalErr "Uplink encountered a fatal internal error."

    --------------------------------------------------
    -- RPC Command API
    --------------------------------------------------

    post "/" $ do
      config <- runNodeT' $ lift NodeState.askConfig
      if Config.rpcReadOnly config
        then jsonReadOnlyErr
        else do
          reqBody <- WS.body
          case A.eitherDecode reqBody of
            Left err -> jsonParseErr $ toS err
            Right val -> do
              resp <- runNodeT' $ handleRPCCmd chan val
              WS.json $ toJSON resp

    --------------------------------------------------
    -- RPC Query API
    --------------------------------------------------

    ---------------------------
    -- Status API
    ---------------------------

    post "/health" $
      WS.status status200

    post "/version" $
      WS.json $ object [
          "version" .= Version.version
        , "branch"  .= Version.branch
        , "commit"  .= Version.commit
        , "dirty"   .= Version.dirty
        ]
    ---------------------------
    -- Blocks API
    ---------------------------

    post "/blocks" $ do
      eBlocks <- runNodeT' getBlocks
      case eBlocks of
        Left err -> jsonInternalErr $ toS err
        Right blocks -> jsonRPCRespM blocks

    post "/blocks/:blockIdx" $ do
      eBlockIdx <- WS.parseParam <$> WS.param "blockIdx"
      case eBlockIdx of
        Left _ -> do
          let errMsg = "Block index should be a positive integer."
          jsonInvalidParamErr errMsg
        Right blockIdx -> do
          eBlock <- runNodeT' $ getBlock blockIdx
          case eBlock of
            Left err -> jsonNotFoundErr $ toS err
            Right block -> jsonRPCRespM block

    ---------------------------
    -- Peers API
    ---------------------------

    post "/peers" $ do
      peers <- runNodeT' NodeState.getPeers
      jsonRPCRespM peers

    post "/peers/validators" $ do
      validatorPeers <- runNodeT' NodeState.getValidatorPeers
      jsonRPCRespM validatorPeers

    ---------------------------
    -- Accounts API
    ---------------------------

    post "/accounts" $
      jsonRPCRespM =<< runNodeT' getAccounts

    post "/accounts/:addr" $ do
      addr <- Address.fromRaw <$> WS.param "addr"
      if Address.validateAddress addr
        then do
          eAcct <- runNodeT' $ getAccount addr
          either (jsonNotFoundErr . toS) jsonRPCRespM eAcct
      else jsonInvalidParamErr "Address supplied is an invalid Sha256 hash"

    ---------------------------
    -- Assets API
    ---------------------------

    post "/assets" $ do
      assetsWithAddrs <- runNodeT' getAssetsWithAddrs
      jsonRPCRespM $ map (uncurry $ Asset.assetWithAddrJSON) assetsWithAddrs

    post "/assets/:addr" $ do
      addr <- Address.fromRaw <$> WS.param "addr"
      if Address.validateAddress addr
        then do
          eAsset <- runNodeT' $ getAsset addr
          either (jsonNotFoundErr . toS) jsonRPCRespM eAsset
      else jsonInvalidParamErr "Address supplied is an invalid Sha256 hash"

    ---------------------------
    -- Contract API
    ---------------------------

    post "/contracts" $
      jsonRPCRespM =<< runNodeT' getContracts

    post "/contracts/:addr" $ do
      addr <- Address.fromRaw <$> WS.param "addr"
      if Address.validateAddress addr
        then do
          eContract <- runNodeT' $ getContract addr
          either (jsonNotFoundErr . toS) jsonRPCRespM eContract
      else jsonInvalidParamErr "Address supplied is an invalid Sha256 hash"

    post "/contracts/:addr/callable" $ do
      addr <- Address.fromRaw <$> WS.param "addr"
      if Address.validateAddress addr
        then do
          eContract <- runNodeT' $ getContract addr
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

    post "/txlog" $ do
      jsonRPCRespM ()

    post "/transactions/pool" $ do
      memPool <- runNodeT' NodeState.getTxMemPool
      jsonRPCRespM memPool

    post "/transactions/pool/size" $ do
      memPool <- runNodeT' NodeState.getTxMemPool
      jsonRPCRespM $ MemPool.size memPool

    post "/transactions/pool/all" $ do
      memPoolMap <- runNodeT' getAllMemPools
      jsonRPCRespM (memPoolMap :: Map Text (Either Text MemPool.MemPool))

    post "/transactions/pool/all/sizes" $ do
      memPoolsSizeMap <- runNodeT' getAllMemPoolSizes
      jsonRPCRespM (memPoolsSizeMap :: Map Text (Either Text Int))

    post "/transactions/:blockIdx" $ do
      eBlockIdx <- WS.parseParam <$> WS.param "blockIdx"
      case eBlockIdx of
        Left _ -> do
          let errMsg = "Block index should be a positive integer."
          jsonInvalidParamErr errMsg
        Right blockIdx -> do
          eTxs <- runNodeT' $ getTransactions blockIdx
          either (jsonNotFoundErr . toS) jsonRPCRespM eTxs

    post "/transactions/:blockIdx/:txIdx" $ do
      eBlockIdx <- WS.parseParam <$> WS.param "blockIdx"
      eTxIdx    <- WS.parseParam <$> WS.param "txIdx"
      case (,) <$> eBlockIdx <*> eTxIdx of
        Left _ -> do
          let errMsg = "Both block and transaction indexs should be positive integers."
          jsonInvalidParamErr errMsg
        Right (blockIdx, txIdx) -> do
          eTx <- runNodeT' $ getTransaction blockIdx txIdx
          either (jsonNotFoundErr . toS) jsonRPCRespM eTx

-- | Construct a RPC response from a serializeable structure
jsonRPCRespM :: ToJSON a => a -> WS.ActionM ()
jsonRPCRespM = WS.json . RPCResp . toJSON

-- | Construct a RPC response indicating success
jsonRPCRespOK :: WS.ActionM ()
jsonRPCRespOK = WS.json $ RPCRespOK

jsonInvalidParamErr :: Text -> WS.ActionM ()
jsonInvalidParamErr = WS.json . RPCRespError . InvalidParam

jsonInternalErr :: Text -> WS.ActionM ()
jsonInternalErr = WS.json . RPCRespError . Internal

jsonParseErr :: Text -> WS.ActionM ()
jsonParseErr = WS.json . RPCRespError . JSONParse

jsonReadOnlyErr :: WS.ActionM ()
jsonReadOnlyErr = WS.json readOnlyErr

jsonNotFoundErr :: Text -> WS.ActionM ()
jsonNotFoundErr = WS.json . RPCRespError . NotFound

-------------------------------------------------------------------------------
-- Querying (Local NodeState)
-------------------------------------------------------------------------------

getBlocks :: MonadIO m => NodeT m (Either [Char] [Block.Block])
getBlocks = NodeState.withBlockDB $ \blockDB ->
  liftIO $ fmap Block.sortBlocks <$> DB.allBlocks blockDB

getBlock :: MonadIO m => Int -> NodeT m (Either [Char] Block.Block)
getBlock idx = NodeState.withBlockDB $ liftIO . flip DB.lookupBlock idx

getAccounts :: MonadIO m => NodeT m [Account.Account]
getAccounts = NodeState.withLedgerState $ pure . Map.elems . Ledger.accounts

getAccount :: MonadIO m => Address.Address -> NodeT m (Either [Char] Account.Account)
getAccount addr = NodeState.withLedgerState $ pure . first show . Ledger.lookupAccount addr

getAssetsWithAddrs :: MonadIO m => NodeT m [(Address.Address, Asset.Asset)]
getAssetsWithAddrs = NodeState.withLedgerState $ pure . Map.toList . Ledger.assets

getAsset :: MonadIO m => Address.Address -> NodeT m (Either [Char] Asset.Asset)
getAsset addr = NodeState.withLedgerState $ pure . first show . Ledger.lookupAsset addr

getContracts :: MonadIO m => NodeT m [Contract.Contract]
getContracts = NodeState.withLedgerState $ pure . Map.elems . Ledger.contracts

getContract :: MonadIO m => Address.Address -> NodeT m (Either [Char] Contract.Contract)
getContract addr = NodeState.withLedgerState $ pure . first show . Ledger.lookupContract addr

getTransactions :: MonadIO m => Int -> NodeT m (Either [Char] [Transaction.Transaction])
getTransactions blockIdx = do
  eBlock <- getBlock blockIdx
  liftIO $ return $ Block.transactions <$> eBlock

getTransaction :: MonadIO m => Int -> Int -> NodeT m (Either [Char] Transaction.Transaction)
getTransaction blockIdx txIdx = do
  eTxs <- getTransactions blockIdx
  return $ case eTxs of
    Left err -> Left err
    Right [] -> Left "No transactions in the specified block"
    Right txs -> case txs `atMay` txIdx of
      Nothing -> Left $ "No transaction at index " ++ show txIdx
      Just tx -> Right tx

-------------------------------------------------------------------------------
-- Querying (Network, using RPC)
-------------------------------------------------------------------------------

getAllMemPoolSizes :: MonadIO m => NodeT m (Map Text (Either Text Int))
getAllMemPoolSizes = queryAllRPC mkPeerUrl
  where
    urlPrefix = "POST http://"
    urlSuffix = ":8545/transactions/pool/size" -- XXX 8545 not guarenteed to be RPC port
    mkPeerUrl hn = urlPrefix <> toS hn <> urlSuffix

getAllMemPools :: MonadIO m => NodeT m (Map Text (Either Text MemPool.MemPool))
getAllMemPools = queryAllRPC mkPeerUrl
  where
    urlPrefix = "POST http://"
    urlSuffix = ":8545/transactions/pool" -- XXX 8545 not guarenteed to be RPC port
    mkPeerUrl hn = urlPrefix <> toS hn <> urlSuffix

queryAllRPC
  :: forall a m. (FromJSON a, MonadIO m)
  => (Text -> Text)
  -> NodeT m (Map Text (Either Text a))
queryAllRPC mkUrl = do
    manager   <- liftIO $ newManager defaultManagerSettings
    peerAddrs <- getPeerAddrs
    requests  <- liftIO $ mapM (parseRequest . toS . mkUrl) peerAddrs
    responseBodys <- liftIO $ mapM (fmap responseBody . flip httpLbs manager) requests
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

handleRPCCmd :: MonadIO m => Chan Cmd.Cmd -> RPCCmd -> NodeT m RPCResponse
handleRPCCmd chan rpcCmd = do
  (mP2PCmd, rpcResp) <- case rpcCmd of

    Transaction tx -> do
      liftIO $ putText $
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
      isTestNode <- lift NodeState.isTestNode
      if not isTestNode then
        return (Nothing, notTestNodeErr)
      else handleTestRPCCmd testCmd

  case mP2PCmd of
    Nothing -> return ()
    Just p2pCmd -> liftIO $ writeChan chan p2pCmd

  return rpcResp

handleTestRPCCmd :: MonadIO m => TestRPCCmd -> NodeT m (Maybe Cmd.Cmd, RPCResponse)
handleTestRPCCmd testRPCCmd =
  case testRPCCmd of

    SaturateNetwork nTxs nSecs -> do
      let p2pCmd = Cmd.Test $ Cmd.SaturateNetwork nTxs nSecs
      return (Just p2pCmd, RPCRespOK)

    ResetMemPools -> do
      let p2pCmd = Cmd.Test $ Cmd.ResetMemPools
      return (Just p2pCmd, RPCRespOK)

-------------------------------------------------------------------------------
-- Protocol
-------------------------------------------------------------------------------

-- | An RPC Command (changes state of server)
data RPCCmd
  = Transaction Transaction.Transaction
  | Test TestRPCCmd
  deriving (Eq, Show, Generic)

data TestRPCCmd
  = SaturateNetwork
    { nTxs  :: Int
    , nSecs :: Int
    }
  | ResetMemPools
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
