{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.P2P.Consensus (
  consensusProc,

  SignBlockMsg(..),
  BlockSigMsg(..),

) where

import Protolude hiding (newChan)

import Control.Monad (fail)
import Control.Monad.Base (liftBase)
import Control.Distributed.Process.Lifted
import Control.Distributed.Process.Lifted.Class

import qualified Data.Set as Set
import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.Serialize as S
import qualified Data.ByteString.Char8 as BSC

import DB
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
import qualified Consensus.Authority.Params as CAP

import System.Random (randomRIO)

-------------------------------------------------------------------------------
-- Consensus Types
-------------------------------------------------------------------------------

-- | A Message asking a node to sign a block
data SignBlockMsg = SignBlockMsg Block.Block
  deriving (Show, Eq, Generic, Typeable)

-- | A block signature from a signing node to a generating node
newtype BlockSigMsg = BlockSigMsg Block.BlockSignature
  deriving (Show, Eq, Generic, Typeable, S.Serialize)

-- | The consensus process is made up of two sub-processes,
-- one for handling SignBlock messages, and another for generating
-- a block given the parameters of the current PoA Consensus alg.
consensusProc
  :: forall m. (MonadReadWriteDB m, MonadProcessBase m)
  => Service
  -> NodeT m ()
consensusProc msgService = do
    Log.info "Consensus process started."

    spawnLocal blockGenProc
    consensusProc'
  where
    -- Process that handles Consensus messages
    consensusProc' :: NodeT m ()
    consensusProc' = do
      controlP $ \runInBase ->
        forever $ do
          receiveWait
            [ match $ runInBase . onSignBlockMsg
            , match $ runInBase . onBlockSigMsg
            ]

    -- SignBlockMsg handler
    onSignBlockMsg :: (SignBlockMsg, ProcessId) -> NodeT m ()
    onSignBlockMsg (SignBlockMsg block, replyTo) = do

      let blockOrigin = Block.origin $ Block.header block
      Log.info $ "Received SignBlockMsg from " <> show blockOrigin

      -- Validate block with respect to ledger state
      ledgerValid <-
        NodeState.withApplyCtx $ \applyCtx ->
          NodeState.withLedgerState $ \world ->
            liftIO $ V.validateBlock applyCtx world block

      case ledgerValid of
        Left err ->
          Log.warning $
            "[Ledger] Block invalid:\n    " <> show err
        Right _ -> do
          eBlockSig <- C.signBlock block
          case eBlockSig of
            Left err ->
              Log.warning $
                "[Consensus] Block invalid:\n    " <> show err
            Right blockSig' -> do

              -- Construct BlockSig message
              selfAddr <- askSelfAddress
              let blockSig = Block.BlockSignature blockSig' selfAddr
              let blockSigMsg = BlockSigMsg blockSig

              -- Reply to Node who asked me to sign the block
              P2P.nsendPeer Service.Consensus (processNodeId replyTo) blockSigMsg

    -- BlockSigMsg handler
    onBlockSigMsg :: BlockSigMsg -> NodeT m ()
    onBlockSigMsg (BlockSigMsg blockSig) = do
      Log.info $ "Received BlockSigMsg from " <> show (Block.signerAddr blockSig)
      mBlock <- C.acceptBlockSig blockSig
      case mBlock of
        Nothing -> pure ()
        Just newBlock -> do
          -- If block has enough signatures, broadcast
          blockMsg <- Msg.mkBlockMsg newBlock
          P2P.nsendPeers' msgService blockMsg

-- | Process that generates new blocks according to the current
-- consensus algorithm parameters in the latest block on the chain
blockGenProc
  :: forall m. (MonadWriteDB m, MonadProcessBase m)
  => NodeT m ()
blockGenProc =
    forever blockGenProc'
  where
    blockGenProc' :: NodeT m ()
    blockGenProc' = do

      lastBlock <- NodeState.getLastBlock

      -- Wait until time to generate next block
      -- Note: If a no valid transaction exist in the mempool, the time since the
      -- last block was generated will far exceed the block period. This will
      -- cause this function to loop without waiting any time at all. A default
      -- wait of 3 seconds [arbitrary] is forced here.
      loginfo "Waiting to create next block..."
      liftIO $ waitToGenNextBlock 3000000 lastBlock

      -- Attempt to generate a new block
      loginfo $ "Trying to create block "
        <> show (Block.index lastBlock + 1)
      eBlock <- C.generateBlock
      case eBlock of
        Left err    -> logwarning $ show err
        Right block -> do

          loginfo $ "Successfully made block..."

          -- Send SignBlock Msgs to all peers
          selfPid <- getSelfPid
          let signBlockMsg = (SignBlockMsg block, selfPid)
          validatorPeers <- NodeState.getValidatorPeers

          loginfo $ "Sending block to all validators to sign..."
          forM_ validatorPeers $ \(Peer peerPid addr) -> do
            let peerNodeId = processNodeId peerPid
            P2P.nsendPeer Service.Consensus peerNodeId signBlockMsg

    -- Wait to generate next block dictated by previous blocks's blockPeriod
    waitToGenNextBlock :: Int64 -> Block.Block -> IO ()
    waitToGenNextBlock nms lastBlock = do
      let lastBlockTs = Block.timestamp $ Block.header lastBlock
      let blockPeriod = CAP.blockPeriod $ Block.getConsensus lastBlock
      let nextBlockTs = lastBlockTs + (secondsToMicroseconds blockPeriod) -- in microsecs

      -- Wait just a bit longer to prevent all generating nodes from making blocks all at once
      randMicroSecs <- randomRIO (1000000, secondsToMicroseconds $ blockPeriod `div` 3)
      let timeToWaitUntil = nextBlockTs + randMicroSecs

      -- If timeToWaitUntil is earlier than "now", wait a default
      -- n micro seconds instead of not waiting at all.
      currentTs <- Time.now
      if timeToWaitUntil <= currentTs
        then Utils.waitUntil $ currentTs + nms
        else Utils.waitUntil timeToWaitUntil

    secondsToMicroseconds = (*) 1000000

    logPref logf = logf . (<>) "[Consensus - blockGenProc] "

    loginfo, logwarning :: Text -> NodeT m ()
    loginfo    = liftIO . logPref Log.info
    logwarning = liftIO . logPref Log.warning


-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

instance B.Binary SignBlockMsg where
  put (SignBlockMsg blk) = do
    B.put $ S.encode blk
  get = do
    eBlk <- S.decode <$> B.get
    case eBlk of
      Left err -> fail err
      Right blk -> pure $ SignBlockMsg blk

instance B.Binary BlockSigMsg where
  put blockSigMsg =
    B.put $ S.encode blockSigMsg
  get = do
    bs <- B.get
    case S.decode bs of
      Left err -> fail err
      Right bsm -> pure bsm
