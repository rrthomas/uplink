{-|

Consensus is the property in which all nodes in a distributed system of replicated
state machines agree upon the current state of their respective state machine. Regarding
DLTs, consensus around the Ledger state and block chain is essential.

In this model, each block in the chain stores the consensus algorithm in which to evaluate
the validity of the next block, as well as the arguments to the consensus algorithm by
which it should be validated. A block is not valid if it's consensus arguments do not provide
the necessary values for which a block is valid. The two important types are:

PoAState:
  A value used by validating nodes during the signing of new blocks. If the new
  block the node is being requested to sign has an index <= prevSignedBlockIdx, then
  the node will not sign the block. After the node signs a new block, this field is
  updated to reflect the latest block signed.

PoA:
  A record representing the M-of-N multisig PoA consensus algorithm parameters. These
  parameters can be tweaked in the chain.config file and determine the constraints in
  which to validate and accept a block when a node receives a new block from a
  signing/generating node.

---------

Note:
  This module contains all the types related to Uplink Consensus.
  The reason that these types are not in the same files as the logic
  associated with their respectful consensus algorithms is because of
  cyclical module imports. Several modules necessary for consensus logic
  also need to import the types found in this module. If the types were found
  in the same module as the consensus logic, the other modules would not be
  able to import the types they need without forming an import cycle.

-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Consensus.Authority.Types (

  PoA(..),
  mkGenesisPoA,

  PoAState(..),
  defPoAState,

  ValidatorSet(..),
  isValidatorAddr,

  PoAError(..),
) where

import Protolude hiding (put, get)

import Control.Monad.Fail (fail)

import Data.Ord (compare)
import qualified Data.Set as Set
import qualified Data.Serialize as S

import Address (Address)
import qualified Config
import qualified Address
import qualified Hash
import qualified Key

-------------------------------------------------------------------------------
-- Proof of Authority (Federated Consensus, M-of-N Multi-signature)
-------------------------------------------------------------------------------

-- | State of validating/generating node with respect to ConsensusAlg
data PoAState = PoAState
  { prevSignedBlockIdx :: Int
  , prevGenBlockIdx    :: Int
  } deriving (Show, Eq, Generic, S.Serialize, Hash.Hashable)

defPoAState :: PoAState
defPoAState = PoAState 0 0

-- | Set of validating nodes that sign blocks
newtype ValidatorSet = ValidatorSet
  { unValidatorSet :: Set.Set Address }
  deriving (Show, Eq, Generic, NFData, S.Serialize, Hash.Hashable)

instance Monoid ValidatorSet where
  mempty = ValidatorSet mempty
  v1 `mappend` v2 = ValidatorSet $
    unValidatorSet v1 `mappend` unValidatorSet v2

isValidatorAddr :: Address -> ValidatorSet ->  Bool
isValidatorAddr addr validators = addr `Set.member` unValidatorSet validators

-- | M-of-N Multi-sig PoA Consensus Parameters
data PoA = PoA
  { validatorSet  :: ValidatorSet -- ^ List of Validating Nodes
  , blockPeriod   :: Int64        -- ^ Minimum time between new blocks (in microseconds)
  , blockGenLimit :: Int          -- ^ # of consecutive blocks of which the validator can generate one block
  , threshold     :: Int          -- ^ # of signatures/votes required for block approval
  , minTxs        :: Int          -- ^ Minimum # of transactions per block
  , signerLimit   :: Int          -- ^ # of consecutive blocks of which the validator can sign one block
  } deriving (Show, Eq, Generic, NFData, S.Serialize, Hash.Hashable)

mkGenesisPoA :: Config.ChainSettings -> Either PoAValidationError PoA
mkGenesisPoA Config.ChainSettings{..} =
    case validatePoA poa of
      Nothing  -> Right poa
      Just err -> Left err
  where
    validatorSet = ValidatorSet $ Set.fromList $
      map (Address.parseAddress . toS) poaValidators

    poa = PoA
      { validatorSet  = validatorSet
      , blockPeriod   = fromIntegral poaBlockPeriod
      , blockGenLimit = poaBlockLimit
      , signerLimit   = poaSignLimit
      , threshold     = poaThreshold
      , minTxs        = poaTransactions
      }

data PoAValidationError
  = PoAValidationError Text
  deriving Show

-- | There are invalid PoA parameter configurations, this function is used to
-- rule out thesse invalid configurations. An Uplink node will not boot if a
-- it has an invalid PoA configuration.
validatePoA :: PoA -> Maybe PoAValidationError
validatePoA PoA{..}
  | blockGenLimit > numValidators = Just $
      PoAValidationError "The blockGenLimit must be less than the # of validators"
  | fromIntegral signerLimit > (fromIntegral numValidators / fromIntegral threshold) = Just $
      PoAValidationError "The signerLimit must be less than (# of validators / threshold)"
  | threshold > numValidators = Just $
      PoAValidationError "The threshold must be less than the # of validators"
  | any (<1) [fromIntegral blockPeriod, blockGenLimit, signerLimit, threshold, minTxs] = Just $
      PoAValidationError "All integer values in the PoA configuration must be greater than 0"
  | otherwise = Nothing
  where
    numValidators = Set.size $ unValidatorSet validatorSet

data PoAError
  = NotEnoughSignatures                -- When block doesn't have enough signatures
  | NotEnoughTransactions              -- When block doesn't have enough transactions
  | InvalidBlockIdx Int Int            -- When prevSignedBlock is >= new block index
  | NoAccountPrivateKey Text           -- When looking up signing key fails
  | BlockGenLimitSurpassed Int         -- When block is generated by a validator within the blockGenLimit
  | BlockSignerLimitSurpassed          -- When block contains a signature from a node that has signed a block too recently
  | InvalidBlockPeriod Int64 Int64     -- When a block is created too quickly
  | AccountDoesNotExist Address        -- An account wiht the given public key does not exist
  | NonValidatingNodeAddress Address   -- A signature from an account with address that does not belong to a validating node
  | NonValidatingNodeOrigin Address    -- The origin of the block is not a validating node
  | InvalidSignature Text              -- When a signature fails to be validated
  | BlockValidationError Text          -- When block fails to validate by chain rules
  | ValidationError Text               -- Constructor to absorb other validation error types
  | PoAErrors [PoAError]               -- Turn a list of errors into a single error
  deriving (Show, Eq)
