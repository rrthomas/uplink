{-|

Ledger world state.

-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Ledger (
  -- ** Ledger state
  World(..),
  AssetError(..),
  AccountError(..),
  ContractError(..),
  genesisWorld,
  mkWorld,

  -- ** Accounts
  lookupAccount,
  accountExists,
  addAccount,
  addAccounts,
  removeAccount,

  -- ** Assets
  lookupAsset,
  assetExists,
  addAsset,
  removeAsset,
  updateAsset,
  transferAsset,
  circulateAsset,

  -- ** Contracts
  lookupContract,
  contractExists,
  addContract,
  updateContract,

) where

import Protolude hiding (from, to, get, put)

import Control.Monad.State
import Control.Arrow ((&&&))

import Data.Aeson (ToJSON, FromJSON)
import Data.Serialize (Serialize, encode)
import qualified Data.Map as Map

import Asset (Asset)
import Account (Account)
import Address (Address)
import Contract (Contract)
import Storage (LocalStorage(..))

import qualified Key
import qualified Asset
import qualified Account
import qualified Contract
import qualified Address
import qualified Storage

-------------------------------------------------------------------------------
-- Ledger State
-------------------------------------------------------------------------------

{-

A distributed ledger is a immutable replicated state machine using cryptography
to commit transactions to a global world state that allows self-reconcilling
systems with inherent non-repudiation.

A ledger world state is a registry of three components:
  - Accounts  ( addresses of participants )
  - Assets    ( associated with addresses )
  - Contracts ( operating over assets, issued by addresses )

A contract is a (addr, code, state) tuple.
  - Address - Global address of deployed code.
  - Code    - Executable logic.
  - State   - Namespace in which logic executes.

State changes are called 'transactions' and are state changes to one of the
three registries.

-}

-- | The world state in-memory.
data World = World
  { contracts :: Map.Map Address Contract
  , assets    :: Map.Map Address Asset
  , accounts  :: Map.Map Address Account.Account
  } deriving (Show, Eq, Generic, NFData, Serialize, ToJSON, FromJSON)

instance Monoid World where
  mempty = genesisWorld
  w1 `mappend` w2 = World
    (contracts w1 `mappend` contracts w2)
    (assets w1    `mappend` assets w2)
    (accounts w1  `mappend` accounts w2)

data ContractError
  = ContractDoesNotExist Address
  | ContractExists Address
  deriving (Show, Eq, Generic, Serialize)

data AssetError
  = AssetDoesNotExist Address
  | AssetExists Address
  | AssetError Asset.AssetError
  | CirculatorIsNotIssuer Address Asset
  | SenderDoesNotExist Address
  | ReceiverDoesNotExist Address
  | RevokerIsNotIssuer Address Asset
  deriving (Show, Eq, Generic, Serialize)

data AccountError
  = AccountDoesNotExist Address
  | AccountExists Address
  deriving (Show, Eq, Generic, Serialize)

-- | Empty world state
genesisWorld :: World
genesisWorld = World mempty mempty mempty

mkWorld :: [Asset] -> [Account] -> [Contract] -> World
mkWorld assets accounts contracts =
    World contractsMap assetsMap accountsMap
  where
    keyValF f = f &&& identity
    valToKeyValMap f = Map.fromList . map (keyValF f)

    assetsMap    = valToKeyValMap Asset.address assets
    accountsMap  = valToKeyValMap Account.address accounts
    contractsMap = valToKeyValMap Contract.address contracts

-------------------------------------------------------------------------------
-- Polymorphic Funcs
-------------------------------------------------------------------------------

lookup :: (World -> Map Address a) -> Address -> World -> Maybe a
lookup f addr = Map.lookup addr . f

exists :: (World -> Map Address a) -> Address -> World -> Bool
exists f addr = isJust . lookup f addr

-------------------------------------------------------------------------------
-- Accounts
-------------------------------------------------------------------------------

lookupAccount :: Address -> World -> Either AccountError Account.Account
lookupAccount addr world =
  case lookup accounts addr world of
    Nothing   -> Left $ AccountDoesNotExist addr
    Just acc  -> Right acc

accountExists :: Address -> World -> Bool
accountExists addr = exists accounts addr

addAccount :: Account.Account -> World -> Either AccountError World
addAccount acct world =
  case lookupAccount (Account.address acct) world of
    Left _ -> Right $
      let accounts' = Map.insert (Account.address acct) acct (accounts world)
      in world { accounts = accounts' }
    Right _ -> Left $ AccountExists (Account.address acct)

addAccounts :: [Account.Account] -> World -> Either AccountError World
addAccounts accs world = foldM (flip Ledger.addAccount) world accs

removeAccount :: Account.Account -> World -> Either AccountError World
removeAccount acct world =
  case lookupAccount (Account.address acct) world of
    Left err -> Left err
    Right _ ->
      let accounts' = Map.delete (Account.address acct) (accounts world)
      in Right $ world { accounts = accounts' }

-------------------------------------------------------------------------------
-- Assets
-------------------------------------------------------------------------------

lookupAsset :: Address -> World -> Either AssetError Asset
lookupAsset addr world =
  case lookup assets addr world of
    Nothing -> Left $ AssetDoesNotExist addr
    Just asset -> Right asset

assetExists :: Address -> World -> Bool
assetExists addr = exists assets addr

-- | Adds a new asset to world state. Fails if the asset already exists.
addAsset :: Address -> Asset -> World -> Either AssetError World
addAsset addr ass world =
  case lookupAsset addr world of
    Left _ -> Right $ updateAsset addr ass world
    Right _ -> Left $ AssetExists addr

-- | Remove an asset from the world
removeAsset :: Address -> World -> Either AssetError World
removeAsset addr world =
  case lookup assets addr world of
    Nothing -> Left $ AssetDoesNotExist addr
    Just asset ->
      let assets' = Map.delete addr (assets world)
      in Right $ world { assets = assets' }

-- | Overwrites the asset at a given address
updateAsset :: Address -> Asset -> World -> World
updateAsset addr ass world =
  let assets' = Map.insert addr ass (assets world) in
  world { assets = assets' }

-- | Transfer holdings of an asset from one holder to another
-- Note: Holder can be a contract address, to hold funds in escrow
transferAsset
  :: Address       -- ^ Asset Address
  -> Address       -- ^ Sender (Origin, From) Address
  -> Address       -- ^ Receiver (To) Address
  -> Asset.Balance -- ^ Amount to send
  -> World         -- ^ World State
  -> Either AssetError World
transferAsset assetAddr from to bal world = do
    validateTransferAddrs world from to
    case lookupAsset assetAddr world of
      Left err -> Left $ AssetDoesNotExist assetAddr
      Right asset -> do
        asset' <- first AssetError $
          Asset.transferHoldings from to bal asset
        Right $ world { assets = Map.insert assetAddr asset' (assets world) }
  where
    validateTransferAddrs
      :: World
      -> Address -- ^ Sender Address (account or contract)
      -> Address -- ^ Receiver Address (account or contract)
      -> Either AssetError ()
    validateTransferAddrs world from to = void $ do
      -- Check if origin account/contract exists
      first (const $ Ledger.SenderDoesNotExist from) $
        case Ledger.lookupAccount from world of
          Left err -> second (const ()) $
            Ledger.lookupContract from world
          Right acc -> Right ()
      -- Check if toAddr account/contract exists
      first (const $ Ledger.ReceiverDoesNotExist to) $
        case Ledger.lookupAccount to world of
          Left err -> second (const ()) $
            Ledger.lookupContract to world
          Right acc -> Right ()

-- | Moves an amount of an asset's supply to the holdings of the asset issuer
circulateAsset
  :: Address        -- ^ Asset Address
  -> Address        -- ^ Origin Address
  -> Asset.Balance  -- ^ Amount to circulate
  -> World          -- ^ World state
  -> Either AssetError World
circulateAsset assetAddr txOrigin amount world =
  case lookupAsset assetAddr world of
    Left err    -> Left $ AssetDoesNotExist assetAddr
    Right asset -> do
      let assetIssuer = Asset.issuer asset
      if assetIssuer /= txOrigin
        then Left $ CirculatorIsNotIssuer txOrigin asset
        else do
          asset' <- first AssetError $
            Asset.circulateSupply assetIssuer amount asset
          Right $ world { assets = Map.insert assetAddr asset' (assets world) }

-------------------------------------------------------------------------------
-- Contracts
-------------------------------------------------------------------------------

lookupContract :: Address -> World -> Either ContractError Contract
lookupContract addr world =
  case lookup contracts addr world of
    Nothing -> Left $ ContractDoesNotExist addr
    Just contract -> Right contract

contractExists :: Address -> World -> Bool
contractExists = exists contracts

-- | Adds a new contract to world state. Fails if contract already exists
addContract :: Address -> Contract -> World -> Either ContractError World
addContract addr contract world =
  case lookupContract addr world of
    Left err -> Right $ updateContract addr contract world
    Right _ -> Left $ ContractExists addr

-- | Unsafe update of contract at address
updateContract :: Address -> Contract -> World -> World
updateContract addr contract world =
  let contracts' = Map.insert addr contract $ contracts world
  in world { contracts = contracts' }
