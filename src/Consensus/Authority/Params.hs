{-|

Consensus is the property in which all nodes in a distributed system of replicated
state machines agree upon the current state of their respective state machine. Regarding
DLTs, consensus around the Ledger state and block chain is essential.

In this model, each block in the chain stores the consensus algorithm in which to evaluate
the validity of the next block, as well as the arguments to the consensus algorithm by
which it should be validated. A block is not valid if it's consensus arguments do not provide
the necessary values for which a block is valid.

PoA:
  A record representing the M-of-N multisig PoA consensus algorithm parameters. These
  parameters can be tweaked in the chain.config file and determine the constraints in
  which to validate and accept a block when a node receives a new block from a
  signing/generating node.

-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Consensus.Authority.Params (

  PoA(..),
  mkGenesisPoA,
  mkGenesisPoA',

  ValidatorSet(..),
  isValidatorAddr,

) where

import Protolude hiding (put, get)

import Control.Monad.Fail (fail)

import Data.Aeson (ToJSON(..))
import Data.Ord (compare)
import qualified Data.Set as Set
import qualified Data.Serialize as S

import Address (Address)
import qualified Address
import qualified Hash
import qualified Key

import Database.PostgreSQL.Simple.ToField   (ToField(..), Action(..))
import Database.PostgreSQL.Simple.FromField (FromField(..), ResultError(..), returnError)

-------------------------------------------------------------------------------
-- Proof of Authority (Federated Consensus, M-of-N Multi-signature)
-------------------------------------------------------------------------------

-- | Set of validating nodes that sign blocks
newtype ValidatorSet = ValidatorSet
  { unValidatorSet :: Set.Set Address }
  deriving (Show, Eq, Generic, NFData, S.Serialize, Hash.Hashable)

instance Monoid ValidatorSet where
  mempty = ValidatorSet mempty
  v1 `mappend` v2 = ValidatorSet $
    unValidatorSet v1 `mappend` unValidatorSet v2

instance ToJSON ValidatorSet where
  toJSON (ValidatorSet vset) = toJSON vset

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
  } deriving (Show, Eq, Generic, NFData, S.Serialize, Hash.Hashable, ToJSON)

mkGenesisPoA
  :: [Text]
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Either PoAValidationError PoA
mkGenesisPoA validators blkPeriod blkLimit sgnLimit thresh mintxs =
    case validatePoA poa of
      Nothing  -> Right poa
      Just err -> Left err
  where
    poa =
      mkGenesisPoA'
        validators
        blkPeriod
        blkLimit
        sgnLimit
        thresh
        mintxs

mkGenesisPoA'
  :: [Text]
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> PoA
mkGenesisPoA' validators blkPeriod blkLimit sgnLimit thresh mintxs =
    PoA
      { validatorSet  = validatorSet
      , blockPeriod   = fromIntegral blkPeriod
      , blockGenLimit = blkLimit
      , signerLimit   = sgnLimit
      , threshold     = thresh
      , minTxs        = mintxs
      }
  where
    validatorSet = ValidatorSet $ Set.fromList $
      map (Address.parseAddress . toS) validators

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

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

-- Necessary instances because Data.Serialize.encode/decode does not play well
-- with postgresql-simple's ByteString-to-bytea serializer
instance ToField PoA where
  toField = EscapeByteA . S.encode

instance FromField PoA where
  fromField f mdata = do
    bs <- fromField f mdata
    case S.decode <$> bs of
      Nothing          -> returnError UnexpectedNull f ""
      Just (Left err)  -> returnError ConversionFailed f err
      Just (Right poa) -> return poa
