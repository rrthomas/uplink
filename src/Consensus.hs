{-|

Consensus service.

--}

module Consensus (
  -- ** Consensus
  acceptBlock,
  validateBlock,
  signBlock,
  generateBlock,
) where

import Protolude

import Control.Monad.Base (liftBase)
import Control.Distributed.Process (Process)

import Data.List (genericLength)
import qualified Data.Set as Set
import qualified Data.Serialize as S

import NodeState
import Block (Block)

import qualified DB
import qualified Key
import qualified Block
import qualified MemPool
import qualified Time
import qualified Validate
import qualified NodeState
import qualified Logging as Log

import Consensus.Authority.Types (PoAState)
import qualified Consensus.Authority as CA
import qualified Consensus.Authority.Types as CAT

-------------------------------------------------------------------------------
-- Validate - PoA Consensus
-------------------------------------------------------------------------------

validateBlock
  :: MonadIO m
  => CA.ValidationCtx
  -> Block.Block
  -> NodeT m ([CAT.PoAError], Bool)
validateBlock validateCtx newBlock = do
  prevBlockPoA <- Block.getConsensus <$> NodeState.getLatestBlock
  CA.validateBlock validateCtx newBlock prevBlockPoA

-------------------------------------------------------------------------------
-- Sign Block - PoA Consensus
-------------------------------------------------------------------------------

signBlock
  :: MonadIO m
  => Block.Block
  -> NodeT m (Either CAT.PoAError Key.Signature)
signBlock block = do

    -- Query Current State
    privKey     <- lift NodeState.askPrivateKey
    poaState    <- NodeState.getPoAState
    worldState  <- NodeState.getLedger
    latestBlock <- NodeState.getLatestBlock

    -- Potentially decrement prevSignedBlockIdx by 1
    maybeDecPrevSignedBlkIdx latestBlock poaState

    -- Validate block against PoA Params from prev block
    (errs, validBlock) <- validateBlock CA.BeforeSigning block
    if validBlock
      then
        NodeState.withApplyCtx $ \applyCtx -> do
          eBlockSig <- liftIO $
            CA.signBlock applyCtx poaState privKey worldState block
          case eBlockSig of
            Left err -> return $ Left err
            Right blockSignature -> do
              NodeState.setPoAState $ setPrevSignedBlockIdx poaState block
              return $ Right blockSignature
      else do
        return $ Left $ CAT.PoAErrors errs

  where
    setPrevSignedBlockIdx :: CAT.PoAState -> Block -> CAT.PoAState
    setPrevSignedBlockIdx poaState' block =
      poaState' { CAT.prevSignedBlockIdx = Block.index block }

    -- If new block has not been accepted since 2*blockPeriod, reset previously
    -- signed block index to allow blocks to be signed at the same height.
    -- * This circumvents certain situations that deadlock the PoA consensus.
    maybeDecPrevSignedBlkIdx :: MonadIO m => Block -> PoAState -> NodeT m ()
    maybeDecPrevSignedBlkIdx latestBlock poaState = do
      now <- liftIO Time.now
      let blockPeriod = CAT.blockPeriod $ Block.getConsensus latestBlock
      let latestBlockTs = Block.getTimestamp latestBlock
      when (now - latestBlockTs >= blockPeriod * 2000000) $
        NodeState.setPoAState $
          poaState { CAT.prevSignedBlockIdx =
            calcNewPrevXBlockIdx latestBlock poaState CAT.prevSignedBlockIdx
          }

-------------------------------------------------------------------------------
-- Generate Block - PoA Consensus
-------------------------------------------------------------------------------

data BlockGenError
  = AccountKeyFailure Text
  | PrevBlockIdxError Text
  | NotEnoughSignatures Int Int
  | EmptyMemPool
  deriving (Show)

-- | Main function for generating a new block.
-- 1) Tries to construct a new block (checks state to see if it can)
-- 2) Queries all validators for signatures on the block
-- 3) Adds block signatures recieved from other validators to block
-- 4) Sets the `prevGenBlockIdx` field to new generated block index
generateBlock
  :: (NodeState.Peers -> Block.Block -> Process [Block.BlockSignature])
  -> NodeProcessT (Either BlockGenError Block.Block)
generateBlock requestSigs = do

    -- Query Current State
    latestBlock <- NodeState.getLatestBlock
    poaState    <- NodeState.getPoAState

    -- Potentially decrement prevGenBlockIdx by 1
    maybeDecPrevGenBlkIdx latestBlock poaState

    -- Try to make new block, given current node state
    eBlock <- makeBlock latestBlock
    case eBlock of
      Left err -> return $ Left err
      Right newBlock' -> do

        -- Ask all Validator Peers to sign block
        validatorPeers <- NodeState.getValidatorPeers
        blockSigs <- liftBase $ requestSigs validatorPeers newBlock'

        -- If there are enough signatures, send block to all peers
        let nBlockSigs = length blockSigs
        let poa = Block.getConsensus latestBlock
        let threshold = CAT.threshold poa
        if (nBlockSigs >= threshold)
          then do
            -- Add Validator signatures to block
            let newBlock = newBlock' { Block.signatures = Set.fromList blockSigs }

            -- Set PoAState prevGenBlockIdx to index of new block so that this
            -- function does not generate a new block of the same index
            NodeState.modifyPoAState_ $ \poaState ->
              poaState { CAT.prevGenBlockIdx = Block.index newBlock }

            return $ Right newBlock
          else return $ Left $
            NotEnoughSignatures nBlockSigs threshold

  where

    -- If new block has not been accepted since 2*blockPeriod, reset previously
    -- generated block index to allow blocks to be generated at this height again.
    -- * This circumvents certain situations that deadlock the PoA consensus.
    maybeDecPrevGenBlkIdx :: Block -> PoAState -> NodeProcessT ()
    maybeDecPrevGenBlkIdx latestBlock poaState = do
      now <- liftIO Time.now
      let blockPeriod = CAT.blockPeriod $ Block.getConsensus latestBlock
      let latestBlockTs = Block.getTimestamp latestBlock
      when (now - latestBlockTs >= blockPeriod * 2000000) $
        NodeState.setPoAState $
          poaState { CAT.prevSignedBlockIdx =
            calcNewPrevXBlockIdx latestBlock poaState CAT.prevGenBlockIdx
          }


-- | Attempts to construct a new block given the current NodeState
makeBlock
  :: MonadIO m
  => Block.Block
  -> NodeT m (Either BlockGenError Block.Block)
makeBlock prevBlock = do
    poaState <- NodeState.getPoAState
    memPool <- NodeState.getTxMemPool
    case validationPred prevBlock poaState memPool of
      Just err -> return $ Left err
      Nothing -> do
        -- Validate and remove invalid txs in mempool, returning valid txs
        (validTxs,txErrs) <- NodeState.pruneTxMemPool

        -- Log Invalid Transactions when making block
        let mkErr = (<>) "[makeBlock] Error:\n    " . show
        mapM_ (Log.warning . mkErr) txErrs

        -- Calc newBlock params
        privKey <- lift NodeState.askPrivateKey
        addr <- lift NodeState.askSelfAddress
        let prevBlockHash = Block.hashBlock prevBlock
        let index = Block.index prevBlock + 1
        let poa = Block.consensus $ Block.header prevBlock

        if length validTxs > 0
          then fmap Right $ liftIO $
            Block.newBlock addr prevBlockHash validTxs index privKey poa
          else pure $ Left EmptyMemPool
  where
    -- Validates criteria to create new block is met
    validationPred
      :: Block.Block
      -> CAT.PoAState
      -> MemPool.MemPool
      -> Maybe BlockGenError
    validationPred prevBlock poaState memPool
      | CAT.prevGenBlockIdx poaState > prevBlockIdx && prevBlockIdx /= 0 =
          Just $ PrevBlockIdxError "prevGenBlockIdx > latestBlockIdx"
      | MemPool.size memPool <= 0 = Just EmptyMemPool
      | otherwise = Nothing
      where
            prevBlockIdx = Block.index prevBlock

-------------------------------------------------------------------------------
-- Accept Block - PoA Consensus
-------------------------------------------------------------------------------

-- | Accept a block:
-- 1) Validate the block w/ respect to chain rules
-- 2) Validate the block w/ respect to consensus
-- 3) Verify/Validate the block w/ respect to world state
-- 4) Apply block txs to ledger state
-- 5) Update NodeState and DB to new ledger state
-- 6) Set "latestBlock" field of NodeState to new block
-- 7) Returns True if block was accepted, False otherwise
acceptBlock :: MonadIO m => Block.Block -> NodeT m Bool
acceptBlock block = do

  -- 1) Validate block w/ respect to chain rules
  validBlock <- NodeState.withBlockDB $ \blockDB -> do
    nBlocks <- liftIO $ DB.lastNBlocks blockDB (Block.index block)
    let medianTimestamp = Block.medianTimestamp =<< nBlocks
    case medianTimestamp of
      Left err -> pure $ Left $
        Block.InvalidMedianTimestamp $ toS err
      Right mts -> do
        prevBlock <- NodeState.getLatestBlock
        liftIO $ Block.validateBlock mts prevBlock block

  case validBlock of
    Left err -> do
      Log.warning $ show err
      return False
    Right _ -> do

      -- 2) Validate block w/ respect to consensus
      (errs, blockIsValid) <- validateBlock CA.BeforeAccept block
      mapM_ (Log.warning . show) errs

      if blockIsValid
        then do
          -- Verify & Validate block w/ respect to world state
          Log.info "Verifying and Validating block before acceptance..."
          NodeState.withApplyCtx $ \applyCtx -> do
            eRes <- NodeState.withLedgerState $ \ledgerState ->
              liftIO $ Validate.verifyAndValidateBlock applyCtx ledgerState block
            case eRes of
              Left err -> do
                Log.warning $ show err
                return False
              Right _ -> do
                -- Apply block transactions to ledger state
                Log.info $ "Applying transactions of block "
                  <> show (Block.index block) <> " to world state..."
                eRes <- NodeState.applyBlock block
                case eRes of
                  -- Log invalid transaction
                  Left err -> do
                    Log.warning $ "Error applying block" <> show err
                    return False
                  -- Or update NodeState & DB with data from new block
                  Right _   -> do
                    -- Remove transactions in Block from local MemPool
                    NodeState.removeTxsFromMemPool $ Block.transactions block
                    -- Sync World with DB
                    NodeState.syncNodeStateWithDBs
                    -- Set Latest block to accepted block
                    NodeState.setLatestBlock block
                    return True
        else do
          let errPrefix = "[AcceptBlock] "
          let errMsg = "Not accepting invalid Block with index "
          let errSuffix = show $ Block.index block
          Log.warning $ errPrefix <> errMsg <> errSuffix
          return False

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

calcNewPrevXBlockIdx :: Block -> PoAState -> (PoAState -> Int) -> Int
calcNewPrevXBlockIdx latestBlock poaState field =
  let prevXBlkIdx = field poaState
      latestBlockIdx = Block.index latestBlock
  in if (prevXBlkIdx <= latestBlockIdx)
       then prevXBlkIdx
       else prevXBlkIdx - 1
