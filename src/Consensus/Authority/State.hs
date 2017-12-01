{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Consensus.Authority.State (
  PoAState(..),
  defPoAState,

  setPrevSignedBlockIdx,
  setPrevGenBlock,
  resetPrevGenBlock,
  addSigToPrevGenBlock,
) where

import Protolude

import qualified Data.Set as Set
import qualified Data.Serialize as S

import Block (Block)
import qualified Block
import qualified Hash

-- PoAState:
--   A value used by validating nodes during the signing and generating of new blocks.
--   If the new block the node is being requested to sign has an
--     index <= prevSignedBlockIdx
--   then the node will not sign the block. After the node signs a new block, this
--   field is updated to reflect the latest block signed. Likewise, when a node
--   generates a block, it will set it's prevGenBlock field to this new block,
--   preventing it from attempting to generate another block at the same height.

-- | State of validating/generating node with respect to ConsensusAlg
data PoAState = PoAState
  { prevSignedBlockIdx :: Int
  , prevGenBlock       :: Maybe  Block
  } deriving (Show, Eq, Generic, S.Serialize, Hash.Hashable)

defPoAState :: PoAState
defPoAState = PoAState 0 Nothing

setPrevSignedBlockIdx :: PoAState -> Block -> PoAState
setPrevSignedBlockIdx poaState block =
  poaState { prevSignedBlockIdx = Block.index block }

setPrevGenBlock :: PoAState -> Block -> PoAState
setPrevGenBlock poaState block =
  poaState { prevGenBlock = Just block }

resetPrevGenBlock :: PoAState -> PoAState
resetPrevGenBlock poaState =
  poaState { prevGenBlock = Nothing }

addSigToPrevGenBlock :: PoAState -> Block.BlockSignature -> PoAState
addSigToPrevGenBlock poaState blockSig =
  case prevGenBlock poaState of
    Nothing -> poaState
    Just prevGenBlk -> do
      let prevGenBlkSigs = Block.signatures prevGenBlk
      let prevGenBlk' = prevGenBlk
            { Block.signatures = Set.insert blockSig prevGenBlkSigs }
      poaState { prevGenBlock = Just prevGenBlk' }
