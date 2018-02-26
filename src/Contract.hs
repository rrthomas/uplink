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
  InvalidMethodName(..),

  callableMethods,

  -- ** Validation
  validateContract,

  -- ** Querying contract state
  lookupVarGlobalStorage,
  lookupContractMethod,

  -- ** Signing
  signContract,
) where

import Protolude hiding (state)

import Time (Timestamp)
import Address (Address, rawAddr)
import Storage (Storage, GlobalStorage, LocalStorage)
import Control.Monad
import qualified Key
import qualified Time
import qualified Hash
import qualified Storage
import qualified Address

import Script (Script, Name, lookupMethod)
import Script.Graph (GraphState(..), initialLabel, terminalLabel)
import qualified Script
import qualified Storage
import qualified Script.Graph as Graph
import qualified Script.Pretty as Pretty
import qualified Script.Parser as Parser
import qualified Script.Typecheck as Typecheck

import Data.Serialize (Serialize, encode, decode)
import qualified Data.Map as Map
import qualified Data.Binary as Binary
import qualified Data.Set as Set

import Data.Aeson (ToJSON(..), object, (.=), (.:))
import Data.Aeson.Types (typeMismatch)
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

instance A.FromJSON LocalStorageVars where
  parseJSON = fmap LocalStorageVars . A.parseJSON

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


-- XXX: Implement full validation
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

data InvalidMethodName
  = MethodDoesNotExist Script.Name
  | MethodNotCallable  Script.Name Graph.GraphState
  deriving (Eq, Show, Generic, NFData, Serialize, Hash.Hashable)

-- | Looks up a method with a given name in a Contract, taking into account the
-- current contract state. I.e. if a contract is in "terminal" state, no methods
-- will be returned.
lookupContractMethod
  :: Script.Name
  -> Contract
  -> Either InvalidMethodName Script.Method
lookupContractMethod nm c =
  case lookupMethod nm (script c) of
    Nothing     -> Left $ MethodDoesNotExist nm
    Just method
      | method `elem` callableMethods c -> Right method
      | otherwise -> Left $ MethodNotCallable nm (state c)

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

instance A.ToJSON Contract where
  toJSON Contract{..} = A.object
    [ "timestamp"     .= timestamp
    , "script"        .= Pretty.prettyPrint script
    , "storage"       .= globalStorage
    , "localStorage"     .= localStorage
    , "localStorageVars" .= localStorageVars
    , "methods"       .= methods
    , "state"         .= state
    , "owner"         .= owner
    , "address"       .= address
    ]


instance A.FromJSON Contract where
  parseJSON = \case
      A.Object v ->
        Contract
          <$> v .: "timestamp"
          <*> (parseScriptJSON =<< v .: "script")
          <*> v .: "storage"
          <*> v .: "localStorage"
          <*> v .: "localStorageVars"
          <*> v .: "methods"
          <*> v .: "state"
          <*> v .: "owner"
          <*> v .: "address"
      invalid -> typeMismatch "Contract" invalid
    where
      parseScriptJSON script =
        case Parser.parseScript script of
          Left err     -> fail $ show err
          Right script -> pure script

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
