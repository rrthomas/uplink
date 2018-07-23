{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}

module Network.P2P.Consensus (

  Consensus(..),

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

import Network.P2P.Service (Service(..), ServiceSpec(..), HList(..))
import Network.P2P.Send (MatchProcBase(..))
import Network.P2P.SignedMsg (nsendPeerSigned, nsendPeersSigned, receiveWaitSigned)
import qualified Network.P2P.Message as Msg

import qualified Consensus as C

-------------------------------------------------------------------------------
-- Consensus Service Definition
-------------------------------------------------------------------------------

type MonadProcReadWriteDB m = (MonadReadWriteDB m, MonadProcessBase m)

-- | The Service representing the block generation process of the consensus service
data ConsensusBlockGen = ConsensusBlockGen
  deriving (Show, Generic, B.Binary)

instance Service ConsensusBlockGen where
  serviceSpec _ = Worker blockGenProc

-- | The Service representing the messaging process of the consensus service
data ConsensusMessaging = ConsensusMessaging
  deriving (Show, Generic, B.Binary)

instance Service ConsensusMessaging where
  serviceSpec _ = Worker consensusMessagingProc

-- | The supervisor service representing the conjunction of both the block
-- generation and messaging handling processes of the consensus service
data Consensus = Consensus
  deriving (Show, Generic, B.Binary)

instance Service Consensus where
  serviceSpec _ = Supervisor (ConsensusBlockGen ::: ConsensusMessaging ::: HNil)

-------------------------------------------------------------------------------
-- Consensus Process
-------------------------------------------------------------------------------

-- | A Message asking a node to sign a block
data SignBlockMsg = SignBlockMsg Block.Block
  deriving (Show, Eq, Generic, Typeable, B.Binary)

-- | A block signature from a signing node to a generating node
newtype BlockSigMsg = BlockSigMsg Block.BlockSignature
  deriving (Show, Eq, Generic, Typeable, B.Binary)

startIfValidator :: MonadProcessBase m => NodeT m () -> NodeT m ()
startIfValidator proc = do
  isValidator <- isValidatingNode
  when isValidator proc

-- Process that handles Consensus messages. This process is only started if the
-- node is a validator.
consensusMessagingProc
  :: forall m. (MonadReadWriteDB m, MonadProcessBase m)
  => NodeT m ()
consensusMessagingProc =
  startIfValidator $
    forever $ do
      receiveWaitSigned
        [ MatchProcBase onSignBlockMsg
        , MatchProcBase onBlockSigMsg
        ]
  where
    -- SignBlockMsg handler
    onSignBlockMsg :: (SignBlockMsg, NodeId) -> NodeT m ()
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
              Log.debug $ "Signing block with index " <> bidxStr <> " from " <> borigStr
              -- Construct BlockSig message
              selfAddr <- askSelfAddress
              let blockSig = Block.BlockSignature blockSig' selfAddr
              let blockSigMsg = BlockSigMsg blockSig

              -- Reply to Node who asked me to sign the block
              nsendPeerSigned replyTo ConsensusMessaging blockSigMsg

    -- BlockSigMsg handler
    onBlockSigMsg :: BlockSigMsg -> NodeT m ()
    onBlockSigMsg (BlockSigMsg blockSig) = do
      Log.debug $ "Received Signature from " <> show (Block.signerAddr blockSig)
      mBlock <- C.acceptBlockSig blockSig
      case mBlock of
        Nothing -> pure ()
        Just newBlock -> do
          -- If block has enough signatures, broadcast
          blockMsg <- Msg.mkBlockMsg newBlock
          nsendPeersSigned Msg.Messaging blockMsg

-- | Process that generates new blocks according to the current
-- consensus algorithm parameters in the latest block on the chain
blockGenProc
  :: forall m. (MonadReadWriteDB m, MonadProcessBase m)
  => NodeT m ()
blockGenProc =
    startIfValidator $
      forever blockGenProc'
  where
    blockGenProc' :: NodeT m ()
    blockGenProc' = do
      -- Attempt to generate a new block
      block <- C.generateBlock
      -- Send SignBlock Msgs to all peers
      selfNid <- getSelfNode
      let signBlockMsg = (SignBlockMsg block, selfNid)
      validatorPeers <- NodeState.getValidatorPeers
      let blockIdx = Block.index block
      forM_ validatorPeers $ \(Peer peerPid addr) -> do
        let peerNodeId = processNodeId peerPid
        Log.debug $ "Sending block " <> show blockIdx <> " to " <> show addr <> " to sign..."
        nsendPeerSigned peerNodeId ConsensusMessaging signBlockMsg
