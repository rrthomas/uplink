{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Network.P2P.Consensus (
  consensusProc,

  SignBlockMsg(..),
  BlockSigMsg(..),

) where

import Protolude hiding (newChan)

import Control.Monad (fail)
import Control.Monad.Base (liftBase)
import Control.Distributed.Process

import qualified Data.Set as Set
import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.Serialize as S
import qualified Data.ByteString.Char8 as BSC

import NodeState
import qualified Key
import qualified Utils
import qualified Block
import qualified NodeState
import qualified Time
import qualified Validate as V
import qualified Logging as Log

import Network.P2P.Service (Service(..))
import qualified Network.P2P.Message as Msg
import qualified Network.P2P.Controller as P2P
import qualified Network.P2P.Service as Service

import qualified Consensus as C
import qualified Consensus.Authority.Types as CAT

import System.Random (randomRIO)

-------------------------------------------------------------------------------
-- Consensus Types
-------------------------------------------------------------------------------

-- | A Message asking a node to sign a block
newtype SignBlockMsg = SignBlockMsg Block.Block
  deriving (Show, Eq, Generic, Typeable, S.Serialize)

-- | A block signature from a signing node to a generating node
newtype BlockSigMsg = BlockSigMsg Block.BlockSignature
  deriving (Show, Eq, Generic, Typeable, S.Serialize)

-- | The consensus process is made up of two sub-processes,
-- one for handling SignBlock messages, and another for generating
-- a block given the parameters of the current PoA Consensus alg.
consensusProc :: Service -> NodeProcessT ()
consensusProc msgService = do
  Log.info "Consensus process started."

  nodeConfig <- lift ask
  nodeState <- get

  liftBase $ spawnLocal $
    runNodeT nodeConfig nodeState $
      blockGenProc msgService

  signBlockProc

-- | Process that signs block when receiving SignBlock messages,
-- or other messages to be exchanged between consensus processes
signBlockProc :: NodeProcessT ()
signBlockProc = do

  nodeConfig <- lift ask
  nodeState <- get

  liftBase $ do
    selfPid <- getSelfPid
    forever $ receiveWait
      [ matchIf (isPeer selfPid) $ \signBlkMsg ->
          runNodeT nodeConfig nodeState $
            onSignBlockMsg signBlkMsg
      ]
  where
    -- Predicate for responding to message
    isPeer :: ProcessId -> (a, SendPort b) -> Bool
    isPeer selfPid (_,sendPort) =
      sendPortProcessId (sendPortId sendPort) /= selfPid

    -- SignBlockMsg handler
    onSignBlockMsg
      :: (SignBlockMsg, SendPort BlockSigMsg)
      -> NodeProcessT ()
    onSignBlockMsg (SignBlockMsg block, replyTo) = do
      -- Validate block with respect to ledger state
      ledgerValid <-
        NodeState.withApplyCtx $ \applyCtx ->
          NodeState.withLedgerState $ \world ->
            liftIO $ V.validateBlock applyCtx world block
      case ledgerValid of
        Left err -> Log.warning $ "[Ledger] Block invalid:\n    " <> show err
        Right _ -> do
          eBlockSig <- C.signBlock block
          case eBlockSig of
            Left err -> Log.warning $
              "[Consensus] Block invalid:\n    " <> show err
            Right blockSig' -> do

              -- Construct BlockSig message
              selfAddr <- lift askSelfAddress
              let blockSig = Block.BlockSignature blockSig' selfAddr
              let blockSigMsg = BlockSigMsg blockSig

              -- Reply to Node asking to sign it
              liftBase $ sendChan replyTo blockSigMsg


-- | Process that generates new blocks according to the current
-- consensus algorithm parameters in the latest block on the chain
blockGenProc :: Service -> NodeProcessT ()
blockGenProc msgService =
    forever blockGenProc'
  where
    loginfo :: MonadIO m => Text -> m ()
    loginfo = Log.info . (<>) "[Consensus - blockGenProc] "

    blockGenProc' :: NodeProcessT ()
    blockGenProc' = do
      latestBlock <- NodeState.getLatestBlock
      loginfo $ "Latest block idx: " <> show (Block.index latestBlock)

      -- Wait until time to generate next block
      -- Note: If a no valid transaction exist in the mempool, the time since the
      -- last block was generated will far exceed the block period. This will
      -- cause this function to loop without waiting any time at all. A default
      -- wait of 3 seconds [arbitrary] is forced here.
      loginfo "Waiting to create next block..."
      liftIO $ waitToGenNextBlock 3000000 latestBlock

      -- Attempt to generate a new block
      loginfo $ "Trying to create block " <> show (Block.index latestBlock + 1)
      eBlock <- C.generateBlock sendSignBlockMsgs
      case eBlock of
        Left err -> Log.warning $ "[Consensus - blockGenProc] " <> show err
        Right newBlock -> do
          -- Send new block to network
          blockMsg <- Msg.mkBlockMsg newBlock
          liftBase $ P2P.nsendPeers' msgService blockMsg

    -- Wait to generate next block dictated by previous blocks's blockPeriod
    waitToGenNextBlock :: Int64 -> Block.Block -> IO ()
    waitToGenNextBlock nms latestBlock = do
      let latestBlockTs = Block.timestamp $ Block.header latestBlock
      let blockPeriod = CAT.blockPeriod $ Block.getConsensus latestBlock
      let nextBlockTs = latestBlockTs + (secondsToMicroseconds blockPeriod) -- in microsecs

      -- Wait just a bit longer to prevent all generating nodes from making blocks all at once
      randMicroSecs <- secondsToMicroseconds <$> randomRIO (1, blockPeriod `div` 3)
      let timeToWaitUntil = nextBlockTs + randMicroSecs

      -- If timeToWaitUntil is earlier than "now", wait a default
      -- n micro seconds instead of not waiting at all.
      currentTs <- Time.now
      if timeToWaitUntil <= currentTs
        then Utils.waitUntil $ currentTs + nms
        else Utils.waitUntil timeToWaitUntil

    -- Send SignBlockMsg to all validator peers
    sendSignBlockMsgs :: NodeState.Peers -> Block.Block -> Process [Block.BlockSignature]
    sendSignBlockMsgs validatorPeers block = do
      let validatorPeerList = Set.toList validatorPeers
      fmap catMaybes $
        forM validatorPeerList $ \vPeer -> do
          (sp, rp) <- newChan
          let blockMsg = SignBlockMsg block
          P2P.nsendPeer Service.Consensus (processNodeId $ peerPid vPeer) (blockMsg, sp)
          blockSigMsg <- receiveChanTimeout 3000000 rp
          case blockSigMsg of
            Nothing -> do
              loginfo $ "No BlockSigMsg from " <> show vPeer
              return Nothing
            Just (BlockSigMsg blockSig) -> do
              loginfo $ "Response BlockSigMsg from " <> show vPeer
              return $ Just blockSig

    secondsToMicroseconds = (*) 1000000

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

-- Necessary because of synchronous block signing protocol
-- and cloud-haskell's use of Data.Binary for typed channels
instance B.Binary SignBlockMsg where
  put signBlockMsg =
    B.put $ S.encode signBlockMsg
  get = do
    bs <- B.get
    case S.decode bs of
      Left err -> fail err
      Right sbm -> pure sbm

instance B.Binary BlockSigMsg where
  put blockSigMsg =
    B.put $ S.encode blockSigMsg
  get = do
    bs <- B.get
    case S.decode bs of
      Left err -> fail err
      Right bsm -> pure bsm
