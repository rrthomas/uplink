{-# LANGUAGE Rank2Types #-}

module Consensus.Authority (
  ValidationCtx(..),
  validateBlock,
  validateBlockSignatures,
  signBlock,

  getPubKey,
) where

import Protolude

import Control.Monad.Base

import Control.Arrow ((&&&))

import Data.Either
import qualified Data.List as List
import qualified Data.Set as Set

import DB
import NodeState (NodeT)
import qualified Account
import qualified Address
import qualified Block
import qualified DB
import qualified Key
import qualified Ledger
import qualified NodeState as NS
import qualified Validate as V

import Consensus.Authority.State (PoAState(..))
import Consensus.Authority.Params (ValidatorSet(..), PoA(..), PoAError(..))

-------------------------------------------------------------------------------
-- Validation - PoA Consensus
-------------------------------------------------------------------------------

data ValidationCtx = BeforeAccept | BeforeSigning

-- | Validate a block given a PoAContext (by nodes about to accept the block):
-- Collects errors that occur during validation and reports them.
validateBlock
  :: MonadReadDB m
  => ValidationCtx       -- ^ Validate Signatures?
  -> Block.Block         -- ^ Block to validate
  -> PoA                 -- ^ PoA params(from prev block)
  -> NodeT m ([PoAError], Bool)
validateBlock validateCtx newBlock poa = do
    prevBlock <- NS.getLastBlock

    -- Verify that block signatures are
    --   1) Properly encoded as ByteStrings
    --   2) Originate from validating nodes priv keys
    (sigErrs, nSigs, signerAddrs) <- do
      let blockSigsAndAddrs = Set.toList $ Block.signatures newBlock
      -- Lookup public keys of validator accounts
      (pubKeyErrs', vPubKeys) <- partitionEithers <$> mapM getPubKey vAddrs
      -- Verify block signatures
      let (blockSigs,signerAddrs) = partitionBlkSigsAndAddrs blockSigsAndAddrs
      let (sigErrs, nValidSigs) = validateBlockSignatures newBlock vAddrs blockSigsAndAddrs
      let allErrs = pubKeyErrs' ++ sigErrs
      return (allErrs, nValidSigs, signerAddrs)

    -- Validate the w/ respect to PoA Consensus params
    isValid <- do
      let blockIdx = Block.index newBlock
      validBlockGenLimit <- validateBlockGenLimit blockIdx
      validBlockSignLimit <- validateSignerLimit blockIdx signerAddrs

      pure $ do

        -- If validating before signing, don't check # sigs
        case validateCtx of
          BeforeAccept  -> validateNumSigs nSigs
          BeforeSigning -> Right ()

        validateMinTxs
        validateBlockPeriod prevBlock
        validateOrigin vAddrs
        validBlockGenLimit
        validBlockSignLimit

    case isValid of
      Left err -> return (sigErrs ++ [err], False)
      Right _   -> return (sigErrs, True)
  where
    vAddrs = Set.toList $ unValidatorSet $ validatorSet poa
    blockOrigin = Block.origin (Block.header newBlock)

    partitionBlkSigsAndAddrs
      :: [Block.BlockSignature]
      -> ([Key.Signature], [Address.Address])
    partitionBlkSigsAndAddrs blockSigsAndAddrs = unzip $
        foldr (\bs acc -> getBlkSigAndAddr bs : acc) [] blockSigsAndAddrs
      where
        getBlkSigAndAddr = Block.signature &&& Block.signerAddr
    -------------------------------------------------------
    -- PoA Consensus Parameter Validation
    -------------------------------------------------------

    -- Validate origin address corresponds to a validating node account
    validateOrigin addrs
      | blockOrigin `elem` addrs = Right ()
      | otherwise = Left $ NonValidatingNodeOrigin blockOrigin

    -- Validate that a block has enough valid signatures
    validateNumSigs n
      | n >= threshold poa = Right ()
      | otherwise = Left NotEnoughSignatures

    -- Validate that the block contains the minimum # of txs
    validateMinTxs
      | length (Block.transactions newBlock) >= minTxs poa = Right ()
      | otherwise = Left NotEnoughTransactions

    -- Validate that the block's timestamp is valid
    validateBlockPeriod prevBlock
      | period >= blockPeriod poa = Right ()
      | otherwise = Left $ InvalidBlockPeriod period (blockPeriod poa)
      where
        period = blockTs - prevBlockTs
        prevBlockTs = Block.timestamp $ Block.header prevBlock
        blockTs = Block.timestamp $ Block.header newBlock

    -- Validate the block gen limit. In practice, `blockGenLimit` should not
    -- surpass # of validators, or consensus will get stuck.
    validateBlockGenLimit :: MonadReadDB m => Int -> NodeT m (Either PoAError ())
    validateBlockGenLimit blockIdx = do
      let n = blockGenLimit poa - 1
      eBlocks <- lift $ DB.readLastNBlocks n
      case eBlocks of
        Left err -> pure $ Left $ BlockValidationError $ show err
        Right blks -> do
          let blockOrigins = map (Block.origin . Block.header) blks
          let currBlkOrigin = Block.origin $ Block.header newBlock
          -- Check if curr block origin is the past n blocks
          case List.findIndex (== currBlkOrigin) blockOrigins of
            Nothing  -> pure $ Right ()
            Just idx -> pure $ Left $
              BlockGenLimitSurpassed $ blockGenLimit poa - idx

    -- Validate that the block signers have not been block signers in the past
    -- `signerLimit` number of blocks. In practice, `signerLimit` should not
    -- surpass (# of validators / # of block signatures required) or consensus
    -- will get stuck.
    validateSignerLimit
      :: MonadReadDB m
      => Int               -- ^ Current Block index
      -> [Address.Address] -- ^ Current Block signer addresses
      -> NodeT m (Either PoAError ())
    validateSignerLimit blockIdx currBlkSignerAddrs = do
      let n = signerLimit poa - 1
      eBlocks <- lift $ DB.readLastNBlocks n
      let eBlockSigners = concatMap (Set.toList . Block.signatures) <$> eBlocks
      case eBlockSigners of
        Left err -> pure $ Left $ BlockValidationError $ show err
        Right pastNBlockSigAddrs -> do
          let signerAddrs = snd $ partitionBlkSigsAndAddrs pastNBlockSigAddrs
          if null (List.intersect currBlkSignerAddrs signerAddrs)
            then pure $ Right ()
            else pure $ Left BlockSignerLimitSurpassed

-- | Validates block signatures, returning a list of errors that occurred
-- during validation, and the total number of valid block signatures. We do not want to
-- stop validation on the first error because this would give attackers too much
-- control. By collecting errors and still succeeding if enough valid signatures
-- are present, we do not allow an invalid signature to inhibit consensus.
validateBlockSignatures
  :: Block.Block            -- ^ Block to verify
  -> [Address.Address]      -- ^ List of Validator Addresses
  -> [Block.BlockSignature] -- ^ List of block signatures
  -> ([PoAError], Int)      -- ^ (List of errors, Number of valid signatures)
validateBlockSignatures block vAddrs blockSigs =
  second length . partitionEithers $
    flip map blockSigs $ \(Block.BlockSignature sig addr) ->
      if addr `elem` vAddrs
        then Right addr
        else Left $ NonValidatingNodeAddress addr
  where
    blockHash = Block.hashBlock block

-- | Get an account's public key by address from the ledger state
getPubKey
  :: MonadBase IO m
  => Address.Address
  -> NodeT m (Either PoAError Key.PubKey)
getPubKey addr = do
  mAcc <- NS.lookupAccount addr
  case mAcc of
    Left _ -> return $ Left $ AccountDoesNotExist addr
    Right acc -> return $ Right $ Account.publicKey acc

-------------------------------------------------------------------------------
-- Sign Block
-------------------------------------------------------------------------------

-- | This function is to be called by a Validating Node after
-- receiving  a block from a generating node and being asked to sign it.
signBlock
  :: V.ApplyCtx
  -> PoAState
  -> Key.PrivateKey
  -> Ledger.World
  -> Block.Block
  -> IO (Either PoAError Key.Signature)
signBlock applyCtx poaState privKey world block = do
    validateBlockWorld <- V.verifyAndValidateBlock applyCtx world block
    let validationRes = do
          -- Validate w/ respect to PoAState (Consensus)
          validateBlockPoAState poaState
          -- Validate w/ respect to World state
          first (ValidationError . show) validateBlockWorld
    case validationRes of
      Left err -> return $ Left err
      Right _  -> Right <$>
        Key.sign privKey (Block.hashBlock block)
  where
    -- Validates the block against the current PoAState before signing
    validateBlockPoAState :: PoAState -> Either PoAError ()
    validateBlockPoAState pstate
      | blockIdx <= psbIdx = Left $ InvalidBlockIdx blockIdx psbIdx
      | otherwise = Right ()
      where
        blockIdx = Block.index block
        psbIdx = prevSignedBlockIdx pstate
