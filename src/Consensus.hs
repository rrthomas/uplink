{-# LANGUAGE ScopedTypeVariables #-}

{-|

Consensus service.

--}

module Consensus (
  -- ** Consensus Validation
  CA.validateBlock,

  -- ** Consensus State
  acceptBlock,
  acceptBlockSig,

  -- ** Consensus Process Actions
  signBlock,
  generateBlock,
  makeBlock,

) where

import Protolude

import Control.Arrow ((&&&))
import Control.Monad.Base (liftBase)

import Control.Distributed.Process.Lifted.Class
import qualified Data.Set as Set

import NodeState
import Block (Block)

import DB.Class
import qualified DB
import qualified Key
import qualified Account
import qualified Block
import qualified Hash
import qualified Ledger
import qualified MemPool
import qualified Time
import qualified Utils
import qualified Validate as V
import qualified Network.P2P.Logging as Log

import Consensus.Authority.State (PoAState(..))
import qualified Consensus.Authority as CA
import qualified Consensus.Authority.Params as CAP
import qualified Consensus.Authority.State  as CAS

import System.Random (randomRIO)

data BlockValidationError
  = ConsensusBlockValidationErr CA.BlockValidationError
  | LedgerBlockValidationErr    Block.InvalidBlock
  deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Sign Block - PoA Consensus
-------------------------------------------------------------------------------

signBlock
  :: (MonadProcessBase m, MonadReadDB m)
  => Block.Block
  -> NodeT m (Either BlockValidationError Key.Signature)
signBlock block = do
    NodeState.withApplyCtx $ \applyCtx -> do

     -- Query NodeState
      poaState <- NodeState.getPoAState
      privKey  <- NodeState.askPrivateKey
      world    <- NodeState.getLedger

      -- Validate and sign the block
      eBlockSig  <- do
        -- Validate w/ respect to World state
        validateBlockWorld <- lift $ do
          let applyState = V.initApplyState world
          V.verifyValidateAndApplyBlock applyState applyCtx block
        -- Validate w/ respect to Consensus model
        (err, validBlock) <- CA.validateBlock CA.BeforeSigning block
        let validateBlockConsensus =
              if validBlock then pure () else Left err
        let validationRes = do
              -- Validate w/ respect to PoAState (Consensus)
              first ConsensusBlockValidationErr $ do
                validateBlockConsensus
                validateBlockIndex poaState
              bimap LedgerBlockValidationErr (const ()) validateBlockWorld
        case validationRes of
          Left err -> pure $ Left err
          Right _  ->
            fmap Right $ liftBase $
              Key.sign privKey (Hash.getRawHash $ Block.hashBlock block)

      case eBlockSig of
        Left err -> return $ Left err
        Right blockSignature -> do
          NodeState.modifyPoAState_ $ flip CAS.setPrevSignedBlockData block
          return $ Right blockSignature
  where
    -- Only sign the block if it's index is > prevSignedBlockIdx
    validateBlockIndex :: PoAState -> Either CA.BlockValidationError ()
    validateBlockIndex pstate
      | blockIdx > psbIdx = Right ()
      | otherwise = Left $ CA.AlreadySignedBlockAtHeight psbIdx
      where
        blockIdx = Block.index block
        psbIdx = fst $ prevSignedBlockData pstate

-------------------------------------------------------------------------------
-- Generate Block - PoA Consensus
-------------------------------------------------------------------------------

-- | Main function for generating a new block:
-- 1) Waits until the block period since the last accepted block has past
-- 2) Potentially resets the previously generated block field to prevent deadlock
-- 3) Tries to construct a new block (checks state to see if it can)
-- 4) Sets the `prevGenBlock` field to the newly generated block
--
-- Note: This function loops until validation predicate is satisfied.
generateBlock
  :: forall m. (MonadProcessBase m, MonadReadWriteDB m)
  => NodeT m Block.Block
generateBlock = do

    prevBlock <- NodeState.getLastBlock
    -- Wait until (latestBlockTimestamp + blockPeriod) to generate next block
    liftIO $ waitToGenNextBlock prevBlock

    -- Potentially reset the PoAState to avoid deadlock
    maybeResetPoAState prevBlock

    -- Validate the node state before attempting to create a new block
    memPool  <- NodeState.getTxMemPool
    poaState <- NodeState.getPoAState
    let poa = Block.getConsensus prevBlock
    currTs   <- liftBase Time.now
    case validationPred prevBlock currTs memPool poaState poa of
      -- If validation pred fails, loop and keep trying
      Just err -> do
        Log.info $ "Not creating block " <> show (Block.index prevBlock + 1) <> ": " <> show err
        -- If a no valid transaction exist in the mempool, the time since the
        -- last block was generated will far exceed the block period. This will
        -- cause this function to loop without waiting any time at all. A default
        -- wait of 10000 microseconds [arbitrary] is forced here, such that the
        -- process is not looping as fast as it possibly can, attempting to
        -- generate a new block.
        waitAndTryAgain
      -- Try to make new block, given current node state
      Nothing -> do
        eBlock <- makeBlock
        case eBlock of
          Left err -> do
            Log.warning $ show err
            waitAndTryAgain
          Right newBlock -> do
            -- Make sure the block is valid
            (err, isBlockValid) <- CA.validateBlock CA.BeforeSigning newBlock
            if isBlockValid
               then do
                 -- Set PoAState prevGenBlockIdx to index of new block so that this
                 -- function does not generate a new block of the same index
                 NodeState.modifyPoAState_ $ flip CAS.setPrevGenBlock newBlock
                 Log.info $ "Successfully created block " <> show (Block.index newBlock) <> "!"
                 pure newBlock
               else do
                 Log.warning $ show err
                 waitAndTryAgain
  where
    -- Waits 100ms before trying again
    waitAndTryAgain :: NodeT m Block.Block
    waitAndTryAgain = liftIO (threadDelay 100000) >> generateBlock

    -- Wait to attempt to generate next block dictated by previous blocks's blockPeriod
    waitToGenNextBlock :: Block.Block -> IO Int64
    waitToGenNextBlock lastBlock = do
      let lastBlockTs = Block.timestamp $ Block.header lastBlock
      let blockPeriod = CAP.blockPeriod $ Block.getConsensus lastBlock
      let nextBlockTs = lastBlockTs + blockPeriod -- in microsecs

      currentTs <- Time.now
      if nextBlockTs <= currentTs
        then pure 0 -- Didn't wait at all
        else Utils.waitUntil nextBlockTs

    -- Validates criteria to create new block is met, reporting a reason in
    -- which the node state does not imply a block should be generated
    validationPred
      :: Block.Block
      -> Time.Timestamp
      -> MemPool.MemPool
      -> CAS.PoAState
      -> CAP.PoA
      -> Maybe CA.BlockValidationError
    validationPred prevBlock currTs memPool poaState poaParams
      -- If the block period has not passed yet
      | currTs <= afterBlockPeriod =
          Just $ CA.InvalidBlockPeriod currTs afterBlockPeriod
      -- If I have already signed a new block at this height
      | prevSignedBlockIdx > prevBlockIdx =
          Just $ CA.AlreadySignedBlockAtHeight prevSignedBlockIdx
      -- If I have already generated a block at this height
      | mPrevGenBlockIdx > Just prevBlockIdx  =
          case mPrevGenBlockIdx of
            Nothing  -> Nothing -- This will never happen
            Just idx -> Just $ CA.BlockWaitingForSigs idx
      -- If there aren't enough transactions in the mempool
      | nTxsInMemPool < nTxsNeeded =
          Just $ CA.NotEnoughTransactions nTxsInMemPool nTxsNeeded
      | otherwise = Nothing
      where
        prevSignedBlockIdx = fst $ CAS.prevSignedBlockData poaState
        mPrevGenBlockIdx = fmap Block.index (CAS.prevGenBlock poaState)
        prevBlockPeriod  = CAP.blockPeriod $ Block.getConsensus prevBlock
        prevBlockIdx     = Block.index prevBlock
        prevBlockTs      = Block.timestamp $ Block.header prevBlock
        afterBlockPeriod = prevBlockTs + prevBlockPeriod
        nTxsInMemPool    = MemPool.size memPool
        nTxsNeeded       = CAP.minTxs poaParams

    -- Resets PoAState iff:
    -- 1) New block has not been accepted since 2*blockPeriod
    -- 2) latestBlock in NodeState has larger index than the prev gen block
    --
    -- Resetting the PoAState entails setting the previously generated block
    -- field to `Nothing`, and decrementing the previously signed block index to
    -- the latest block accepted index.
    --
    -- * This circumvents certain situations that deadlock the PoA consensus.
    maybeResetPoAState :: Block -> NodeT m ()
    maybeResetPoAState prevBlock = do
        (PoAState prevSignedBlkData mCurrGenBlock) <- NodeState.getPoAState
        now <- liftBase Time.now
        case mCurrGenBlock of
          Nothing
            -- If now is later than the block period after the prev signed block ts
            | now > (snd prevSignedBlkData) + blockPeriod -> do
                Log.warning "Resetting poaState because of potential deadlock..."
                -- Wait for a random amount of time before attempting to gen a new block
                resetPoAStateAndWait
            | otherwise -> pure ()
          Just currGenBlock
            -- Reset the prev gen block field because block of same height
            -- or greater has already been applied to the node ledger state
            | newerLatestBlock currGenBlock -> do
                Log.info $ "Resetting poaState because next block has been generated..."
                resetPoAState
            -- If there has been two block periods passing without a block of
            -- the same height as this node's previously generated block, reset
            | blockPeriodHasPassed now (Block.getTimestamp currGenBlock) -> do
                Log.warning "Resetting poaState because of potential deadlock..."
                -- Wait for a random amount of time before attempting to gen a new block
                resetPoAStateAndWait
            -- Otherwise, don't reset the `prevGenBlock` field
            | otherwise -> pure ()

      where
        resetPoAStateAndWait = do
          resetPoAState
          waitedFor <- liftBase waitRandom
          Log.info $ "Waited for " <> show waitedFor <> "us."

        resetPoAState =
          NodeState.setPoAState $
            CAS.PoAState (Block.index &&& Block.getTimestamp $ prevBlock) Nothing

        blockPeriod = CAP.blockPeriod $ Block.getConsensus prevBlock
        prevBlockTs = Block.getTimestamp prevBlock

        -- Has one block period (+1 sec) passed since the last block generated?
        blockPeriodHasPassed currTs prevGenBlockTs =
          currTs >= prevGenBlockTs + blockPeriod + 1000000

        newerLatestBlock currGenBlock =
          Block.index prevBlock >= Block.index currGenBlock

        -- Wait a Random amount of time between 100ms and 1100ms
        waitRandom = do
          randMicroSecs <- randomRIO (100000, 1100000)
          currTs <- Time.now
          Utils.waitUntil $ currTs + randMicroSecs

-- | Constructs a new block given the current state of the node
makeBlock
  :: (MonadProcessBase m, MonadReadWriteDB m)
  => NodeT m (Either CA.BlockValidationError Block.Block)
makeBlock = do
  -- Validate and remove invalid txs in mempool, returning valid txs
  (validTxs,txErrs) <- NodeState.pruneTxMemPool
  -- Add invalid txs to invalidTxPool and DB
  eRes <- appendInvalidTxPool txErrs
  either (Log.warning . show) pure eRes

  -- Log invalid transactions (Block creation still happens if invalid txs)
  mapM_ (Log.warning . show) txErrs

  -- Calc newBlock params
  privKey   <- NodeState.askPrivateKey
  addr      <- NodeState.askSelfAddress
  prevBlock <- NodeState.getLastBlock
  let prevBlockHash = Block.hashBlock prevBlock
  let index = Block.index prevBlock + 1
  let poa = Block.consensus $ Block.header prevBlock

  -- Make block if enough transactions
  let numValidTxs = length validTxs
      numTxsNeeded = CAP.minTxs poa
  if numValidTxs >= numTxsNeeded
     then do
       Log.info $ "Generating block " <> show index <> "..."
       fmap Right $ liftBase $
         Block.newBlock addr prevBlockHash validTxs index privKey poa
     else pure $ Left $ CA.NotEnoughTransactions numValidTxs numTxsNeeded

-------------------------------------------------------------------------------
-- Accept Block - PoA Consensus
-------------------------------------------------------------------------------

-- | Accept a block:
-- 1) Validate the block w/ respect to chain rules
-- 2) Validate the block w/ respect to consensus
-- 3) Verify/Validate the block w/ respect to world state
-- 4) Apply block txs to ledger state
-- 5) Update NodeState and DB to new ledger state
-- 6) Set "prevBlock" field of NodeState to new block
-- 7) Returns True if block was accepted, False otherwise
acceptBlock
  :: (MonadProcessBase m , MonadReadWriteDB m)
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
        Block.InvalidBlock (Block.index block) $
          Block.InvalidMedianTimestamp $ toS err
      Right mts -> do
        prevBlock <- NodeState.getLastBlock
        pure $ Block.validateBlock mts prevBlock block

  case validBlock of
    Left err -> do
      Log.warning $ show err
      return False
    Right _ -> do

      -- 2) Validate block w/ respect to consensus
      (err, blockIsValid) <- CA.validateBlock CA.BeforeAccept block
      if blockIsValid
        then do
          -- Apply block transactions to ledger state
          let blockIdx = Block.index block
          Log.info $ "Verifying, validating, and applying transactions of block " <> show blockIdx
          eRes <- NodeState.verifyValidateAndApplyBlock block
          case eRes of
            Left err -> do
              Log.critical $ "Error applying block " <> show blockIdx <> ":\n  " <> show err
              return False
            Right _   -> return True
        else do
          let errPrefix = "[acceptBlock] Not accepting invalid Block with index "
          let errBlkIdx = show $ Block.index block
          let errMsg    = ":\n   " <> show err
          Log.critical $ errPrefix <> errBlkIdx <> errMsg
          return False

-- | If the block signature is a valid signature of the previously generated
-- block, add the signtature to the previously generated block (in node state).
acceptBlockSig :: MonadProcess m => Block.BlockSignature -> NodeT m (Maybe Block)
acceptBlockSig blkSig@(Block.BlockSignature sig signerAddr) = do
    mCurrGenBlock <- prevGenBlock <$> NodeState.getPoAState
    case mCurrGenBlock of

      Nothing -> do
        Log.warning "[acceptBlockSig]: Currently no block to add signature to."
        return Nothing

      Just currGenBlock -> do

        -- Lookup block signer account
        ledger <- NodeState.getLedger
        case Ledger.lookupAccount signerAddr ledger of
          Left err  -> Log.warning $ show err
          Right acc -> do

            -- Verify block signature
            let accPubKey = Account.publicKey acc
            case Block.verifyBlockSig accPubKey sig currGenBlock of
              -- If signature invalid, report error
              Left err -> Log.warning $ show $
                Block.InvalidBlock (Block.index currGenBlock) $
                  Block.InvalidBlockSignature err
              -- Otherwise add it to the block
              Right _  -> do
                NodeState.modifyPoAState_ $
                  flip CAS.addSigToPrevGenBlock blkSig

        -- If block has enough sigs, return it
        prevBlock  <- NodeState.getLastBlock
        if (blockHasEnoughSigs prevBlock currGenBlock)
          then pure $ Just currGenBlock
          else pure Nothing

  where
    -- Tests if the block has enough signatures based on Consensus params
    -- present in the header of the previous block in the chain
    blockHasEnoughSigs prevBlock currBlock = (==)
      (CAP.threshold $ Block.getConsensus prevBlock)
      (Set.size $ Block.signatures currBlock)
