{-|

Consensus service.

--}

module Consensus (
  -- ** Consensus Validation
  validateBlock,

  -- ** Consensus State
  acceptBlock,
  acceptBlockSig,

  -- ** Consensus Process Actions
  signBlock,
  generateBlock,
  makeBlock,

) where

import Protolude

import Control.Monad.Base (liftBase, MonadBase)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Distributed.Process (Process)

import Data.List (genericLength)
import qualified Data.Set as Set
import qualified Data.Serialize as S

import NodeState
import Block (Block)

import DB.Class
import qualified DB
import qualified Key
import qualified Account
import qualified Block
import qualified Ledger
import qualified MemPool
import qualified Time
import qualified Validate
import qualified NodeState
import qualified Logging as Log

import Consensus.Authority.State (PoAState(..))
import qualified Consensus.Authority as CA
import qualified Consensus.Authority.Params as CAP
import qualified Consensus.Authority.State  as CAS

-------------------------------------------------------------------------------
-- Validate - PoA Consensus
-------------------------------------------------------------------------------

-- | Validates the block given the consensus params in th previous blk header
validateBlock
  :: MonadReadDB m
  => CA.ValidationCtx
  -> Block.Block
  -> NodeT m ([CAP.PoAError], Bool)
validateBlock validateCtx newBlock = do
  prevBlockPoA <- Block.getConsensus <$> NodeState.getLastBlock
  CA.validateBlock validateCtx newBlock prevBlockPoA

-------------------------------------------------------------------------------
-- Sign Block - PoA Consensus
-------------------------------------------------------------------------------

signBlock
  :: MonadReadDB m
  => Block.Block
  -> NodeT m (Either CAP.PoAError Key.Signature)
signBlock block = do

    -- Query Current State
    privKey     <- NodeState.askPrivateKey
    poaState    <- NodeState.getPoAState
    worldState  <- NodeState.getLedger
    latestBlock <- NodeState.getLastBlock

    -- Potentially decrement prevSignedBlockIdx by 1
    maybeDecPrevSignedBlkIdx latestBlock poaState

    -- Validate block against PoA Params from prev block
    (errs, validBlock) <- validateBlock CA.BeforeSigning block
    if validBlock
      then
        NodeState.withApplyCtx $ \applyCtx -> do
          eBlockSig <- liftBase $
            CA.signBlock applyCtx poaState privKey worldState block
          case eBlockSig of
            Left err -> return $ Left err
            Right blockSignature -> do
              NodeState.setPoAState $
                CAS.setPrevSignedBlockIdx poaState block
              return $ Right blockSignature
      else do
        return $ Left $ CAP.PoAErrors errs

  where
    -- If new block has not been accepted since 2*blockPeriod, reset previously
    -- signed block index to allow blocks to be signed at the same height.
    -- * This circumvents certain situations that deadlock the PoA consensus.
    maybeDecPrevSignedBlkIdx :: MonadBase IO m => Block -> PoAState -> NodeT m ()
    maybeDecPrevSignedBlkIdx latestBlock poaState = do
      now <- liftBase Time.now
      let blockPeriod = CAP.blockPeriod $ Block.getConsensus latestBlock
      let latestBlockTs = Block.getTimestamp latestBlock
      when (now - latestBlockTs >= blockPeriod * 2000000) $
        NodeState.setPoAState $
          poaState { CAS.prevSignedBlockIdx =
            calcNewPrevXBlockIdx latestBlock poaState CAS.prevSignedBlockIdx
          }

-------------------------------------------------------------------------------
-- Generate Block - PoA Consensus
-------------------------------------------------------------------------------

data BlockGenError
  = AccountKeyFailure Text
  | PrevBlockIdxError Text
  | NotEnoughSignatures Int Int
  | NoTransactionsInMemPool
  deriving (Show)

-- | Main function for generating a new block.
-- 1) Tries to construct a new block (checks state to see if it can)
-- 2) Sets the `prevGenBlockIdx` field to new generated block index
generateBlock
  :: (MonadBaseControl IO m, MonadWriteDB m)
  => NodeT m (Either BlockGenError Block.Block)
generateBlock = do

    -- Query Current State
    latestBlock <- NodeState.getLastBlock
    poaState    <- NodeState.getPoAState

    -- Potentially decrement prevGenBlockIdx by 1
    maybeResetPrevGenBlock latestBlock poaState

    -- Try to make new block, given current node state
    eBlock <- makeBlock latestBlock
    case eBlock of
      Left err -> return $ Left err
      Right newBlock -> do

        -- Set PoAState prevGenBlockIdx to index of new block so that this
        -- function does not generate a new block of the same index
        NodeState.modifyPoAState_ $
          flip CAS.setPrevGenBlock newBlock

        pure $ Right newBlock
  where

    -- Resets prevGenBlock field of PoAState iff:
    -- 1) New block has not been accepted since 2*blockPeriod
    -- 2) latestBlock in NodeState has larger index
    --
    -- * This circumvents certain situations that deadlock the PoA consensus.
    maybeResetPrevGenBlock :: MonadBase IO m => Block -> PoAState -> NodeT m ()
    maybeResetPrevGenBlock latestBlock poaState = do
        now <- liftBase Time.now
        mCurrGenBlock <- CAS.prevGenBlock <$> NodeState.getPoAState
        case mCurrGenBlock of
          Nothing -> pure ()
          Just currGenBlock ->
            when (pastTwoBlockPeriods now || newerLatestBlock currGenBlock) $
              NodeState.modifyPoAState_ CAS.resetPrevGenBlock
      where
        blockPeriod = CAP.blockPeriod $ Block.getConsensus latestBlock
        latestBlockTs = Block.getTimestamp latestBlock

        pastTwoBlockPeriods currTs =
          currTs - latestBlockTs >= blockPeriod * 2000000

        newerLatestBlock currGenBlock =
          Block.index latestBlock >= Block.index currGenBlock

-- | Attempts to construct a new block given the current NodeState
makeBlock
  :: (MonadBaseControl IO m, MonadWriteDB m)
  => Block
  -> NodeT m (Either BlockGenError Block.Block)
makeBlock prevBlock = do
    poaState  <- NodeState.getPoAState
    memPool   <- NodeState.getTxMemPool
    case validationPred prevBlock poaState memPool of
      Just err -> return $ Left err
      Nothing -> do

        -- Validate and remove invalid txs in mempool, returning valid txs
        (validTxs,txErrs) <- NodeState.pruneTxMemPool
        -- Add invalid txs to invalidTxPool and DB
        eRes <- appendInvalidTxPool txErrs
        either (Log.warning . show) pure eRes

        -- Log invalid transactions (Block creation still happens if invalid txs)
        let mkErr = (<>) "[makeBlock] Error:\n    " . show
        mapM_ (Log.warning . mkErr) txErrs

        -- Calc newBlock params
        privKey <- NodeState.askPrivateKey
        addr <- NodeState.askSelfAddress
        let prevBlockHash = Block.hashBlock prevBlock
        let index = Block.index prevBlock + 1
        let poa = Block.consensus $ Block.header prevBlock

        -- Make block if enough transactions
        if length validTxs > 0
          then fmap Right $ liftBase $
            Block.newBlock addr prevBlockHash validTxs index privKey poa
          else pure $ Left NoTransactionsInMemPool
  where
    -- Validates criteria to create new block is met
    validationPred
      :: Block.Block
      -> CAS.PoAState
      -> MemPool.MemPool
      -> Maybe BlockGenError
    validationPred prevBlock poaState memPool
      |    fmap Block.index (CAS.prevGenBlock poaState) > Just prevBlockIdx
        && prevBlockIdx /= 0 =
          Just $ PrevBlockIdxError "prevGenBlockIdx > latestBlockIdx"
      | MemPool.size memPool <= 0 = Just NoTransactionsInMemPool
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
acceptBlock
  :: (MonadBaseControl IO m , MonadReadWriteDB m)
  => Block.Block
  -> NodeT m Bool
acceptBlock block = do

  -- 1) Validate block w/ respect to chain rules
  validBlock <- do
    medianTimestamp <- lift $ do
      eNBlocks <- DB.readLastNBlocks (Block.index block)
      case eNBlocks of
        Left err      -> pure $ Left $ show err
        Right nBlocks -> pure $ Block.medianTimestamp nBlocks
    case medianTimestamp of
      Left err -> pure $ Left $
        Block.InvalidMedianTimestamp $ toS err
      Right mts -> do
        prevBlock <- NodeState.getLastBlock
        liftBase $ Block.validateBlock mts prevBlock block

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
              liftBase $ Validate.verifyAndValidateBlock applyCtx ledgerState block
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

                  Left err -> do
                    Log.warning $ "Error applying block: " <> show err
                    return False

                  Right _   -> do
                    -- Sync World with DB
                    eSyncRes <- NodeState.syncNodeStateWithDBs
                    case eSyncRes of
                      Left err -> do
                        Log.warning $ "Error syncing to database: " <> show err
                        return False
                      Right _  -> return True
        else do
          let errPrefix = "[AcceptBlock] "
          let errMsg = "Not accepting invalid Block with index "
          let errSuffix = show $ Block.index block
          Log.warning $ errPrefix <> errMsg <> errSuffix
          return False

-- | If the block signature is a valid signature of the previously generated
-- block, add the signtature to the previously generated block (in node state).
acceptBlockSig :: MonadBase IO m => Block.BlockSignature -> NodeT m (Maybe Block)
acceptBlockSig blkSig@(Block.BlockSignature sig signerAddr) = do
    mCurrGenBlock <- prevGenBlock <$> NodeState.getPoAState
    case mCurrGenBlock of

      Nothing -> do
        Log.warning "[acceptBlockSig]: Currently no block to add signature to."
        return Nothing

      Just currGenBlock -> do

        ledger       <- NodeState.getLedger
        case Ledger.lookupAccount signerAddr ledger of
          Left err  -> Log.warning $ show err
          Right acc -> do

            -- Report error or add sig to block
            let accPubKey = Account.publicKey acc
            let eValidBlkSig = Block.verifyBlockSig accPubKey sig currGenBlock
            case eValidBlkSig of
              Left err -> do
                Log.warning $ show err
              Right _  -> do
                NodeState.modifyPoAState_ $
                  flip CAS.addSigToPrevGenBlock blkSig

        -- If block has enough sigs, return it
        latestBlock  <- NodeState.getLastBlock
        if (blockHasEnoughSigs latestBlock currGenBlock)
          then pure $ Just currGenBlock
          else pure Nothing

  where
    blockHasEnoughSigs prevBlock currBlock = (==)
      (CAP.threshold $ Block.getConsensus prevBlock)
      (Set.size $ Block.signatures currBlock)



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
