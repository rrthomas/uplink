{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.P2P.Consensus (
  consensusProc,

  SignBlockMsg(..),
  BlockSigMsg(..),

) where

import Protolude hiding (link)

import Control.Distributed.Process.Lifted
import Control.Distributed.Process.Lifted.Class

import qualified Data.Binary as B

import DB
import NodeState
import Node.Peer
import qualified Block
import qualified Validate as V
import qualified Network.P2P.Logging as Log

import Network.P2P.Service (Service(..))
import Network.P2P.Send (MatchProcBase(..))
import Network.P2P.SignedMsg (nsendPeerSigned, nsendPeersSigned, receiveWaitSigned)
import qualified Network.P2P.Message as Msg
import qualified Network.P2P.Service as Service

import qualified Consensus as C

-------------------------------------------------------------------------------
-- Consensus Types
-------------------------------------------------------------------------------

-- | A Message asking a node to sign a block
data SignBlockMsg = SignBlockMsg Block.Block
  deriving (Show, Eq, Generic, Typeable, B.Binary)

-- | A block signature from a signing node to a generating node
newtype BlockSigMsg = BlockSigMsg Block.BlockSignature
  deriving (Show, Eq, Generic, Typeable, B.Binary)

-- | The consensus process is made up of two sub-processes,
-- one for handling SignBlock messages, and another for generating
-- a block given the parameters of the current PoA Consensus alg.
consensusProc
  :: forall m. (MonadReadWriteDB m, MonadProcessBase m)
  => Service
  -> NodeT m ()
consensusProc msgService = do
    register "consensusproc" =<< getSelfPid
    Log.info "Consensus process started."

    link =<< spawnLocal blockGenProc
    consensusProc'
  where
    -- Process that handles Consensus messages
    consensusProc' :: NodeT m ()
    consensusProc' = do
      forever $ do
        receiveWaitSigned
          [ MatchProcBase onSignBlockMsg
          , MatchProcBase onBlockSigMsg
          ]

    -- SignBlockMsg handler
    onSignBlockMsg :: (SignBlockMsg, ProcessId) -> NodeT m ()
    onSignBlockMsg (SignBlockMsg block, replyTo) = do

      -- Validate block with respect to ledger state
      ledgerValid <-
        NodeState.withApplyCtx $ \applyCtx ->
          NodeState.withLedgerState $ \ledgerState -> lift $ do
            let applyState = V.initApplyState ledgerState
            V.verifyValidateAndApplyBlock applyState applyCtx block

      let bidxStr = show $ Block.index block
      let borigStr = show $ Block.origin $ Block.header block
      let mkErrorMsg err = mconcat
            [ "Not signing invalid block ", bidxStr
            , " from ", borigStr, ":\n    ", show err
            ]

      case ledgerValid of
        Left err -> Log.warning $ mkErrorMsg err
        Right _  -> do
          eBlockSig <- C.signBlock block
          case eBlockSig of
            Left err -> Log.warning $ mkErrorMsg err
            Right blockSig' -> do
              Log.info $ "Signing block with index " <> bidxStr <> " from " <> borigStr
              -- Construct BlockSig message
              selfAddr <- askSelfAddress
              let blockSig = Block.BlockSignature blockSig' selfAddr
              let blockSigMsg = BlockSigMsg blockSig

              -- Reply to Node who asked me to sign the block
              nsendPeerSigned Service.Consensus (processNodeId replyTo) blockSigMsg

    -- BlockSigMsg handler
    onBlockSigMsg :: BlockSigMsg -> NodeT m ()
    onBlockSigMsg (BlockSigMsg blockSig) = do
      Log.info $ "Received Signature from " <> show (Block.signerAddr blockSig)
      mBlock <- C.acceptBlockSig blockSig
      case mBlock of
        Nothing -> pure ()
        Just newBlock -> do
          -- If block has enough signatures, broadcast
          blockMsg <- Msg.mkBlockMsg newBlock
          nsendPeersSigned msgService blockMsg

-- | Process that generates new blocks according to the current
-- consensus algorithm parameters in the latest block on the chain
blockGenProc
  :: forall m. (MonadReadWriteDB m, MonadProcessBase m)
  => NodeT m ()
blockGenProc = do
    register "blockgenproc" =<< getSelfPid
    forever blockGenProc'
  where
    blockGenProc' :: NodeT m ()
    blockGenProc' = do
      -- Attempt to generate a new block
      block <- C.generateBlock
      -- Send SignBlock Msgs to all peers
      selfPid <- getSelfPid
      let signBlockMsg = (SignBlockMsg block, selfPid)
      validatorPeers <- NodeState.getValidatorPeers
      let blockIdx = Block.index block
      forM_ validatorPeers $ \(Peer peerPid addr) -> do
        let peerNodeId = processNodeId peerPid
        Log.info $ "Sending block " <> show blockIdx <> " to " <> show addr <> " to sign..."
        nsendPeerSigned Service.Consensus peerNodeId signBlockMsg
