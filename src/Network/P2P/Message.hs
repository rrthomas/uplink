{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.P2P.Message (
  version,

  -- ** Types
  Message(..),
  SendTransactionMsg(..),

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

import Control.Exception.Lifted (catch)
import Control.Distributed.Process.Lifted.Class (MonadProcessBase, liftP)
import Control.Distributed.Process.Lifted hiding (Message, catch, handleMessage)

import Data.Serialize as S hiding (expect)
import qualified Data.Binary as B
import qualified Data.ByteString as BS

import Block (Block)
import Transaction (Transaction)
import qualified Network.P2P.Logging as Log
import Network.P2P.Service
import Network.P2P.SignedMsg (expectSigned, nsendPeerSigned)
import Network.Utils (mkNodeId, extractNodeId)
import NodeState

import DB.Class
import qualified DB
import qualified Hash
import qualified Block
import qualified Utils
import qualified Validate
import qualified Consensus
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
  } deriving (Show, Generic, Serialize, Typeable, B.Binary, Hash.Hashable)

data BlockMsg = BlockMsg
  { block  :: Block      -- ^ New Block
  , sender :: SafeString -- ^ Message sender NodeId
  } deriving (Show, Generic, Serialize, Typeable, B.Binary, Hash.Hashable)

data GetTxMsg = GetTxMsg
  { hash    :: SafeString
  , blockIx :: Int
  } deriving (Show, Generic, Serialize, Typeable, B.Binary, Hash.Hashable)

data GetBlockMsg = GetBlockMsg
  { index  :: Int         -- ^ Index of block
  , sender :: SafeString  -- ^ Message sender NodeId
  } deriving (Show, Generic, Serialize, Typeable, B.Binary, Hash.Hashable)

-- | Message type used for P2P communication with other nodes.
data Message
  = SendTx SendTransactionMsg           -- tx
  | GetTx GetTxMsg                      -- Request a transaction
  | Ping SafeString                     -- ByteString == sender NodeId
  | Pong SafeString                     -- ByteString == sender NodeId
  | Block BlockMsg                      -- Send a block
  | Version SafeString                  -- Request version
  | VerAck SafeString                   -- Acknowledge version
  | Test TestMessage
  | GetBlock GetBlockMsg                -- Request a block by index
  deriving (Show, Generic, Serialize, Typeable, B.Binary, Hash.Hashable)

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
  } deriving (Show, Generic, Serialize, B.Binary, Hash.Hashable)

data ResetMemPoolMsg = ResetMemPoolMsg
  deriving (Show, Generic, Serialize, B.Binary, Hash.Hashable)

-- | A datatype defining the messages that should only be able to be
-- executed when the node is booted in a test state with '--test'
data TestMessage
  = ServiceRestart ServiceRestartMsg
  | ResetMemPool ResetMemPoolMsg
  deriving (Show, Generic, Serialize, B.Binary, Hash.Hashable)

-------------------------------------------------------------------------------
-- P2P Messaging Process
-------------------------------------------------------------------------------

-- | Message handling service
-- Expects a bytestring because the canonical serialization library used in this
-- software is Data.Serialize, whereas in order to use cloud haskell's `match`
-- function to receive typed values, a Data.Binary instance must be written.
messagingProc
  :: (MonadReadWriteDB m, MonadProcessBase m)
  => Service
  -> NodeT m ()
messagingProc service = do
  NodeEnv nodeConfig nodeState <- ask
  forever $ do
    eRes <- expectSigned
    case eRes of
      Left err -> Log.warning $ show err
      Right msg -> handleMessage service msg

-- | Handle a message datastream, parsingg into the appropriate wire protocol
-- message and then dispatching to Node logic to write the entity to world state.
-- Expects a bytestring because the canonical serialization library used in this
-- software is Data.Serialize, whereas in order to use cloud haskell's `match`
-- function to receive typed values, a Data.Binary instance must be written.
handleMessage
  :: (MonadReadWriteDB m, MonadProcessBase m)
  => Service
  -> Message
  -> NodeT m ()
handleMessage replyService msg = do
  Log.info $ "Recieved Message:\n   " <> show msg
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
          nsendPeerSigned replyService nodeId response

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
          isValidatingNode <- NodeState.isValidatingNode
          when isValidatingNode $ do
            success <- NodeState.appendTxMemPool tx
            if success
               then Log.info "Added Transaction to MemPool..."
               else Log.warning "Duplicate Transaction, not adding to MemPool..."

    Block (BlockMsg block sender) -> do
      success <- Consensus.acceptBlock block
      when success $ do
        blkAtIdxMsg <- mkGetBlockAtIdxMsg (Block.index block + 1)
        mReplyToNodeId <- liftIO $ mkNodeId $ toBytes sender
        case mReplyToNodeId of
          Left err            -> Log.warning err
          Right replyToNodeId ->
            nsendPeerSigned replyService replyToNodeId blkAtIdxMsg

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
              nsendPeerSigned replyService nodeId blockMsg

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
