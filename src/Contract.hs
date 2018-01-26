{-|

Contract datatypes, signing and operations.

-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}

module Contract (
  -- ** Types
  Contract(..),
  LocalStorageVars(..),

  callableMethods,

  -- ** Validation
  validateContract,
  lookupVarGlobalStorage,

  -- ** Signing
  signContract,
) where

import Protolude

import Time (Timestamp)
import Address (Address, rawAddr)
import Storage (Storage, GlobalStorage, LocalStorage)
import Control.Monad
import qualified Key
import qualified Time
import qualified Hash
import qualified Storage
import qualified Address

import Script (Script, Name)
import Script.Graph (GraphState(..), initialLabel, terminalLabel)
import qualified Script
import qualified Storage
import qualified Script.Graph as Graph
import qualified Script.Pretty as Pretty
import qualified Script.Typecheck as Typecheck

import Data.Serialize (Serialize, encode, decode)
import qualified Data.Map as Map
import qualified Data.Binary as Binary
import qualified Data.Set as Set

import Data.Aeson (ToJSON(..), object, (.=))
import qualified Data.Aeson as A

import Database.PostgreSQL.Simple.ToField   (ToField(..), Action(..))
import Database.PostgreSQL.Simple.FromField (FromField(..), ResultError(..), returnError)

-------------------------------------------------------------------------------
-- Contracts
-------------------------------------------------------------------------------

type LocalStorages = Map.Map Address.Address LocalStorage

newtype LocalStorageVars = LocalStorageVars
  { unLocalStorageVars :: Set.Set Name
  } deriving (Eq, Ord, Show, Generic, NFData, Serialize, Hash.Hashable)

instance Monoid LocalStorageVars where
  mempty = LocalStorageVars mempty
  (LocalStorageVars lsvars1) `mappend` (LocalStorageVars lsvars2) =
    LocalStorageVars (lsvars1 `mappend` lsvars2)

instance ToJSON LocalStorageVars where
  toJSON (LocalStorageVars lsvars) = toJSON lsvars

-- | A contract is a 4-tuple of the address of publisher, timestamp of
-- deployment time, script source, and it's initial storage hash.
data Contract = Contract
  { timestamp        :: Timestamp        -- ^ Timestamp of issuance
  , script           :: Script           -- ^ Underlying contract logic
  , globalStorage    :: GlobalStorage    -- ^ Initial state of the contract
  , localStorage     :: LocalStorages    -- ^ Initial state of the contract
  , localStorageVars :: LocalStorageVars -- ^ Names of all local variables
  , methods          :: [Script.Name]    -- ^ Public methods
  , state            :: Graph.GraphState -- ^ State of Contract
  , owner            :: Address          -- ^ Creator of the contract
  , address          :: Address          -- ^ Contract Address, derived during creation
  } deriving (Eq, Show, Generic, NFData, Serialize, Hash.Hashable)


-- XXX: Implement full tests
validateContract :: Contract -> IO Bool
validateContract Contract {..} = do
  return $ and
    [ (length methods) > 0
    , isRight (Typecheck.signatures script)
    ]

-- | Digitally sign a contract with a private key.
signContract :: Key.PrivateKey -> Contract -> IO Key.Signature
signContract = Key.signS

lookupVarGlobalStorage :: ByteString -> Contract -> Maybe Script.Value
lookupVarGlobalStorage k c = Map.lookup (Storage.Key k) gs
  where
    gs = Storage.unGlobalStorage $ globalStorage c

-- | Returns the list of callable methods given the current contract state
callableMethods :: Contract -> [Script.Method]
callableMethods c =
    filter ((==) cStateLabel . methodLabel) $
      Script.scriptMethods (script c)
  where
    cStateLabel = case Contract.state c of
      GraphLabel lbl -> lbl
      GraphInitial   -> initialLabel
      GraphTerminal  -> terminalLabel

    methodLabel m =  case Script.methodTag m of
      Script.Main lbl -> lbl
      Script.Subg lbl -> lbl

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

instance A.ToJSON Contract where
  toJSON contract = A.object
    [ "timestamp"     .= timestamp contract
    , "script"        .= Pretty.prettyPrint (script contract)
    , "storage"       .= globalStorage contract
    , "methods"       .= fmap Script.unName (methods contract)
    , "state"         .= Contract.state contract
    -- Extra field for ease of use in SDK
    , "terminated"    .= (Contract.state contract == Graph.GraphTerminal)
    , "owner"         .= owner contract
    , "address"       .= address contract
    ]

instance Binary.Binary Contract where
  put tx = Binary.put $ encode tx
  get = do
    bs <- Binary.get
    case decode bs of
      (Right tx) -> return tx
      (Left err) -> fail err

instance ToField LocalStorageVars where
  toField = EscapeByteA . Data.Serialize.encode

instance FromField LocalStorageVars where
  fromField f mdata = do
    bs <- fromField f mdata
    case Data.Serialize.decode <$> bs of
      Nothing              -> returnError UnexpectedNull f ""
      Just (Left err)      -> returnError ConversionFailed f err
      Just (Right lvarnms) -> return lvarnms
