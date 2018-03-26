{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.P2P.Message (
  version,

  -- ** Types
  Message(..),
  SyncHeadMsg(..),
  SendTransactionMsg(..),
  MsgType(..),

  messagingProc,

  TestMessage(..),
  ServiceRestartMsg(..),
  ResetMemPoolMsg(..),
  handleTestMessage,

  -- ** Message Construction
  mkBlockMsg,
  mkGetBlockAtIdxMsg,

  -- ** Message Bodies
  BlockMsg(..),
  GetPoolMsg(..),
  GetBlockMsg(..),

  -- ** Serialization
  encodeMsg,
  decodeMsg,
  debugMsg,

  -- ** Size Querying
  sizeMsg,
  sizeMb,

) where

import Protolude hiding (get,put,catch)

import Control.Monad.Base (liftBase)

import Control.Exception.Lifted (catch)
import Control.Distributed.Process.Lifted.Class (MonadProcessBase, liftP, controlP)
import Control.Distributed.Process.Lifted hiding (Message, catch, handleMessage)

import Data.Serialize as S hiding (expect)
import qualified Data.Binary as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Text.Encoding.Error (lenientDecode)

import Key (PubKey)
import Block (Block)
import Transaction (Transaction)
import qualified Network.P2P.Logging as Log
import Network.P2P.Service
import Network.P2P.Controller
import Network.Utils (mkNodeId, extractNodeId)
import NodeState

import DB.Class
import qualified DB
import qualified Block
import qualified Utils
import qualified Validate
import qualified Consensus
import qualified NodeState
import qualified Transaction

import SafeString

{-
NetMsgType:

version
verack
addr
inv
getdata
merkleblock
getblocks
getheaders
tx
headers
block
getaddr
mempool
notfound
reject
sendheaders
sendcmpct
cmpctblock
getblocktxn
blocktxn

-}

version :: SafeString
version = "0.1a"

-------------------------------------------------------------------------------
-- Wire Protocol (Messages)
-------------------------------------------------------------------------------

-- (pk, addr, tx)
data SendTransactionMsg = SendTransactionMsg
  { transaction :: Transaction
  } deriving (Show, Generic, Serialize)

-- (head)
data SyncHeadMsg = SyncHeadMsg
  { head :: ByteString
  , index :: Int
  } deriving (Show, Generic, Serialize)

data BlockMsg = BlockMsg
  { block  :: Block      -- ^ New Block
  , sender :: SafeString -- ^ Message sender NodeId
  } deriving (Show, Generic, Serialize)

data GetTxMsg = GetTxMsg
  { hash :: ByteString
  , blockIx :: Int
  } deriving (Show, Generic, Serialize)

data GetPoolMsg = GetPoolMsg
  { inv :: Int
  , pool :: [Transaction]
  } deriving (Show, Generic, Serialize)

data GetBlockMsg = GetBlockMsg
  { index  :: Int         -- ^ Index of block
  , sender :: SafeString  -- ^ Message sender NodeId
  } deriving (Show, Generic, Serialize)

-- | Message type used for P2P communication with other nodes.
data Message
  = SendTx SendTransactionMsg           -- tx
  | SyncHead SyncHeadMsg                -- getblocks
  | NotFound                            -- notfound

  | GetTx GetTxMsg                      -- Request a transaction

  | MemPool GetPoolMsg                  -- mempool

  | Ping SafeString                     -- ByteString == sender NodeId
  | Pong SafeString                     -- ByteString == sender NodeId

  | Block BlockMsg                      -- Send a block

  | Version SafeString                  -- Request version
  | VerAck SafeString                   -- Acknowledge version

  | Test TestMessage

  | GetBlock GetBlockMsg                -- Request a block by index

  | Malformed SafeString                -- Message was malformed
  deriving (Show)

mkBlockMsg :: MonadProcessBase m => Block.Block -> NodeT m Message
mkBlockMsg blk = do
  nodeIdSS <- fmap fromBytes' $ liftP extractNodeId
  pure $ Block $ BlockMsg blk nodeIdSS

mkGetBlockAtIdxMsg :: MonadProcessBase m => Int -> NodeT m Message
mkGetBlockAtIdxMsg idx = do
  nodeIdSS <- fromBytes' <$> liftP extractNodeId
  let getBlkAtMsg = GetBlockMsg idx nodeIdSS
  pure $ GetBlock getBlkAtMsg

-------------------------------------------------------------------------------
-- Wire Protocol (TestMessages)
-------------------------------------------------------------------------------

data ServiceRestartMsg = ServiceRestartMsg
  { serviceName :: Service
  , delay       :: Int
  } deriving (Show, Generic, Serialize)

data ResetMemPoolMsg = ResetMemPoolMsg
  deriving (Show, Generic, Serialize)

-- | A datatype defining the messages that should only be able to be
-- executed when the node is booted in a test state with '--test'
data TestMessage
  = ServiceRestart ServiceRestartMsg
  | ResetMemPool ResetMemPoolMsg
  | MalformedTestMsg SafeString
  deriving (Show)

-- | Message protocol
--
-- There are two message types:
--
-- 1. Simple: message sent to a single peer
-- 2. Broadcast: messages sent to the entire network, iteratively sending
-- messages to neighbors.
data MsgType
  = Simple
  | Broadcast
  deriving (Eq, Show, Generic, Serialize)

-------------------------------------------------------------------------------
-- P2P Messaging Process
-------------------------------------------------------------------------------

-- | Message handling service
-- Expects a bytestring because the canonical serialization library used in this
-- software is Data.Serialize, whereas in order to use cloud haskell's `match`
-- function to receive typed values, a Data.Binary instance must be written.
messagingProc
  :: forall m. (MonadReadWriteDB m, MonadProcessBase m)
  => Service
  -> NodeT m ()
messagingProc service = do
  NodeEnv nodeConfig nodeState <- ask
  controlP $ \runInBase ->
    forever $ runInBase $
      handleMessage service =<< expect

-- | Handle a message datastream, parsingg into the appropriate wire protocol
-- message and then dispatching to Node logic to write the entity to world state.
-- Expects a bytestring because the canonical serialization library used in this
-- software is Data.Serialize, whereas in order to use cloud haskell's `match`
-- function to receive typed values, a Data.Binary instance must be written.
handleMessage
  :: (MonadReadWriteDB m, MonadProcessBase m)
  => Service
  -> ByteString
  -> NodeT m ()
handleMessage replyService msg =
  case decodeMsg msg of
    Left err -> Log.warning ("Malformed message:" <> show err)
    Right msg -> do
      Log.info $ "Recieved Message: \n\t" <> show msg
      case msg of

        Ping sender -> do
          mNodeId <- liftIO $ mkNodeId (SafeString.toBytes sender)
          case mNodeId of
            Left err     ->
              Log.warning $ "Received invalid hostname ping: " <> show err
            Right nodeId -> do
              Log.info $ "Got a ping from " <> show sender
              myNodeId <- liftP $ toS <$> extractNodeId
              let response = Pong (SafeString.fromBytes' myNodeId)
              nsendPeer' replyService nodeId response

        Pong sender -> do
          mNodeId <- liftIO $ mkNodeId (SafeString.toBytes sender)
          case mNodeId of
            Left err     ->
              Log.warning $ "Received invalid hostname pong: " <> show err
            Right nodeId ->
              Log.info $ "Got a pong from: " <> show nodeId

        SendTx (SendTransactionMsg tx) -> do
          ledgerState <- NodeState.getLedger
          let val = do
                    Validate.validateTransactionOrigin ledgerState tx
                    Validate.verifyTransaction ledgerState tx
          case val of
            Left err -> Log.warning $ mconcat
              [ "Could not verify transaction with signature: "
              , show (Transaction.signature tx)
              , " due to: "
              , show err
              ]

            Right _ -> do
              isUnique <- NodeState.elemTxMemPool tx
              if isUnique
                then do
                  isValidatingNode <- NodeState.isValidatingNode
                  when isValidatingNode $ do
                    Log.info "Adding Transaction to MemPool"
                    NodeState.appendTxMemPool tx
                else
                  Log.warning "Duplicate Transaction! Dropping it."

        Block (BlockMsg block sender) -> do
          success <- Consensus.acceptBlock block
          when success $ do
            blkAtIdxMsg <- mkGetBlockAtIdxMsg (Block.index block + 1)
            mReplyToNodeId <- liftIO $ mkNodeId $ toBytes sender
            case mReplyToNodeId of
              Left err            -> Log.warning err
              Right replyToNodeId ->
                nsendPeer' replyService replyToNodeId blkAtIdxMsg

        GetBlock gmsg@(GetBlockMsg idx sender) -> do
          eBlock <- lift $ DB.readBlock idx
          case eBlock of
            Left err  -> Log.warning $ "No block with index " <> show idx
            Right blk -> do
              Log.info $ show gmsg
              eNodeId <- liftIO $ mkNodeId (toBytes sender)
              case eNodeId of
                Left err -> Log.warning err
                Right nodeId -> do
                  blockMsg <- mkBlockMsg blk
                  nsendPeer' replyService nodeId blockMsg

        -- For Messages operating on a "test node"
        Test testMsg -> handleTestMessage replyService testMsg

        _   -> do
          Log.critical (show msg)
          pass


-- | Handler for TestMessages when node is operating in "test" mode
handleTestMessage :: MonadProcessBase m => Service -> TestMessage -> NodeT m ()
handleTestMessage service testMsg = case testMsg of

  ServiceRestart (ServiceRestartMsg service delay) -> do
    Log.info $ "Restarting '" <> show service <> "' service."
    serviceRestart service $ max 0 delay
    Log.info "Service running."

  ResetMemPool _ -> do
    Log.info "Resetting MemPool"
    NodeState.resetTxMemPool

  MalformedTestMsg err -> Log.warning (show err)

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- XXX Write code to restart all restartable processes,
-- Take advantage of Control.Distributed.Process.Managed to restart
serviceRestart :: MonadProcessBase m => Service -> Int -> NodeT m ()
serviceRestart service delay = case service of
  TestMessaging -> do
    selfPid <- liftP getSelfPid
    service' <- liftP $ whereis $ show service
    case service' of
      Nothing -> Log.warning $ mconcat
        ["No service with name ", show service ," exists on this node."]
      Just pid
        | selfPid == pid -> Log.warning "Cannot restart self"
        | otherwise -> do
            let exitCmd = exit pid ("Restarting..." :: ByteString)
            catch (liftP exitCmd) $ \(_ :: SomeException) -> do
              liftIO $ threadDelay delay
              -- XXX Restart process for real...
              Log.info "Successfully restarted process"
  _ -> Log.warning "Can't restart process, not implemented."


-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

{-# INLINABLE putCode #-}
putCode :: Message -> Put
putCode msg = case msg of
  SendTx _     -> putWord16be 0
  SyncHead _   -> putWord16be 1
  NotFound     -> putWord16be 2
  GetBlock  _  -> putWord16be 3
  GetTx _      -> putWord16be 4
  MemPool _    -> putWord16be 5
  Ping _       -> putWord16be 6
  Pong _       -> putWord16be 7
  Version _    -> putWord16be 8
  VerAck _     -> putWord16be 9
  Block _      -> putWord16be 10
  Test _       -> putWord16be 11
  Malformed _  -> panic "Cannot construct malformed message"

putVersion :: Put
putVersion = putWord16be 0x01

maxInv :: Int
maxInv = 500

instance Serialize Message where
    put msg = case msg of
      SendTx tx   -> do
        putCode msg
        put tx

      SyncHead hash -> do
        putCode msg
        put hash

      NotFound -> do
        putCode msg
        pure ()

      GetBlock msg' -> do
        putCode msg
        put msg'

      GetTx hash -> do
        putCode msg
        put hash

      MemPool payload -> do
        putCode msg
        put (inv payload)
        mapM_ put (pool payload)

      Ping n -> do
        putCode msg
        put n

      Pong n -> do
        putCode msg
        put n

      Version n -> do
        putCode msg
        put n

      VerAck n -> do
        putCode msg
        put n

      Block blk -> do
        putCode msg
        put blk

      Test testMsg -> do
        putCode msg
        put testMsg

      Malformed _ -> do
        panic "Cannot construct malformed message"
        pure ()

    get = do
      code <- getWord16be
      case code of

        -- SendTx
        0 -> do
          tx <- get
          pure (SendTx tx)

        -- SyncHead
        1 -> do
          head <- get
          pure (SyncHead head)

        -- NotFound
        2 -> do
          pure NotFound

        -- NotFound
        3 -> do
          body <- get
          pure (GetBlock body)

        -- GetTx
        4 -> do
          body <- get
          pure (GetTx body)

        -- MemPool
        5 -> do
          inv <- get
          if (inv > 0) && (inv < maxInv)
            then do
              txs <- replicateM inv get
              pure (MemPool (GetPoolMsg inv txs))
            else pure (Malformed (fromBytes' $ "Invalid inventory size: " <> show inv))

        -- Ping
        6 -> do
          msg <- get
          pure (Ping msg)

        -- Pong
        7 -> do
          msg <- get
          pure (Pong msg)

        -- Version
        8 -> do
          msg <- get
          pure (Version msg)

        -- VerAck
        9 -> do
          msg <- get
          pure (VerAck msg)

        -- Block
        10 -> do
          body <- get
          pure (Block body)

        -- Test
        11 -> do
          body <- get
          pure (Test body)

        _ -> pure (Malformed "Invalid message code. Exepcted 1-11.")


{-# INLINABLE putTestMsgCode #-}
putTestMsgCode :: TestMessage -> Put
putTestMsgCode testMsg = putWord16be $
  case testMsg of
    ServiceRestart _   -> 0
    ResetMemPool _     -> 1
    MalformedTestMsg _ -> panic "Cannot construct malformed message"

instance Serialize TestMessage where
  put testMsg = do
    putTestMsgCode testMsg
    case testMsg of
      ServiceRestart svr   -> put svr
      ResetMemPool reset   -> put reset
      MalformedTestMsg msg -> panic "Cannot construct malformed message"

  get = do
    code <- getWord16be
    case code of
      0 -> ServiceRestart <$> get
      1 -> ResetMemPool <$> get
      _ -> pure $ MalformedTestMsg "Invalid message code"

-------------------------------------------------------------------------------

-- | Serialize a message
decodeMsg :: ByteString -> Either [Char] Message
decodeMsg = S.decode

-- | Deserialize a message
encodeMsg :: Message -> ByteString
encodeMsg = S.encode

debugMsg :: Message -> [Word8]
debugMsg s = Utils.toByteList (encode s)

sizeMsg :: Message -> Int
sizeMsg = BS.length . encode

sizeMb :: Message -> Double
sizeMb s = (10e-6) * (fromIntegral (BS.length (encode s)))
