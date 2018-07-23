{-|

Adress datatypes and operations.

-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeInType #-}

module Address (
  -- ** Types
  Address(..),
  rawAddr,
  fromRaw,
  emptyAddr,
  shortAddr,
  addrSize,

  -- ** Tagging
  AContract,
  AAccount,
  AAsset,
  showAddr,
  EitherAccountContract,

  -- ** Parsing
  parseAddress,

  -- ** FromField
  addressFromField,

  -- ** Validation
  deriveHash,
  deriveAddress,
  verifyAddress,
  recoverAddress,
  recoverAddress',

  putAddress,
  getAddress,

  -- ** Generation
  newAddr,
  newAddrs,
  newPair,
  newTriple,

) where

import Prelude (Show(..), show)
import Data.Typeable
import Protolude hiding (Show, show)
import qualified GHC.TypeLits as Lits
import Utils (panicImpossible)
import qualified Encoding
import qualified Key
import qualified Hash
import Script.Pretty (Pretty(..), squotes)

import Crypto.Random.Types (MonadRandom(..))
import Data.Aeson (Value(..), ToJSON(..), FromJSON(..), ToJSONKey(..), FromJSONKey(..))
import Data.Aeson.Types (typeMismatch, toJSONKeyText, FromJSONKeyFunction(..))
import Data.Serialize (Serialize)
import qualified Data.Binary as B
import qualified Data.Serialize as S
import qualified Data.ByteString as BS

import Database.PostgreSQL.Simple.FromRow   (FromRow(..))
import Database.PostgreSQL.Simple.ToRow     (ToRow(..))
import Database.PostgreSQL.Simple.ToField   (ToField(..))
import Database.PostgreSQL.Simple.FromField (FromField(..), ResultError(..), returnError)
import Database.PostgreSQL.Simple.Internal (Field, Conversion)

-------------------------------------------------------------------------------
-- Address
-------------------------------------------------------------------------------

-- | Size of a base58 encoded bytestring address
addrSize :: Int
addrSize = Hash.hashSize

-- | A ledger address, derived from elliptic curve point
newtype Address a = Address (Hash.Hash Encoding.Base58ByteString)
  deriving (Show, Read, Eq, Ord, Generic, NFData, B.Binary, Typeable, Hash.Hashable)

instance ToJSON (Address a) where
  toJSON (Address b) = toJSON b

instance FromJSON (Address a) where
  parseJSON v = Address <$> parseJSON v

instance ToJSONKey (Address a) where
  toJSONKey = toJSONKeyText (decodeUtf8 . rawAddr)

instance FromJSONKey (Address a) where
  fromJSONKey = FromJSONKeyTextParser parser
    where
      parser t =
        case Hash.parseRawHash (toS t) of
          Left err -> typeMismatch "Address" (String t)
          Right h  -> pure $ Address h

instance Serialize (Address a) where
  put (Address b) = S.put b
  get = Address <$> S.get

instance Pretty (Address a) where
  ppr (Address b) = squotes $ ppr $ Hash.getHash b

-- | Type level tags of address type
data AContract
data AAccount
data AAsset

instance Show AAccount where
  show = panicImpossible $ Just "Cannot evaluate address tag"
instance Eq AAccount where
  (==) = panicImpossible $ Just "Cannot evaluate address tag"

instance Show AContract where
  show = panicImpossible $ Just "Cannot evaluate address tag"
instance Eq AContract where
  (==) = panicImpossible $ Just "Cannot evaluate address tag"

instance Show AAsset where
  show = panicImpossible $ Just "Cannot evaluate address tag"
instance Eq AAsset where
  (==) = panicImpossible $ Just "Cannot evaluate address tag"

type family AddrTag i where
  AddrTag AContract = "c"
  AddrTag AAccount  = "u"
  AddrTag AAsset    = "a"
  AddrTag _         = Lits.TypeError (Lits.Text "Cannot lower tag of unknown address")

type family EitherAccountContract a :: Constraint where
  EitherAccountContract AAccount = ()
  EitherAccountContract AContract = ()
  EitherAccountContract _ = Lits.TypeError (Lits.Text "Expecting either an account or contract")

-- | Lower the typelevel tag of an address to it's display value.
{-showAddr :: forall a. (KnownSymbol (AddrTag a)) => (Address a) -> Text-}
showAddr :: forall a. (KnownSymbol (AddrTag a)) => Address a -> Text
showAddr addr = toS (symbolVal (Proxy :: Proxy (AddrTag a))) <> toS (show addr)

putAddress :: Address a -> S.PutM ()
putAddress (Address base) = S.put base

getAddress :: S.Get (Address a)
getAddress =  Address <$> S.get

-- | Extract underlying bytes from an 'Address' in base58 encoding
rawAddr :: Address a -> ByteString
rawAddr (Address b) = Hash.getRawHash b

-- | Build an 'Address' from a raw base 58 encoded bytestring. ( Not safe )
fromRaw :: ByteString -> Address a
fromRaw bs =
    case Encoding.parseEncodedBS bs of
      Left err -> panicErr err
      Right (b58bs :: Encoding.Base58ByteString) ->
        case Hash.parseHash b58bs of
          Left err -> panicErr err
          Right h  -> Address h
  where
    panicErr err = panic $
      "Cannot validate address as input to 'fromRaw': " <> toS (show err)

-- | Empty address
emptyAddr :: Address a
emptyAddr = Address Hash.emptyHash

-- | Shortened address for logging
shortAddr :: Address a -> ByteString
shortAddr (Address b) = BS.take 7 (Hash.getRawHash b)

-- | Derive an address from a public key
--
-- > address(x,y) = addrHash(string(x) <> string(y))
deriveAddress :: Key.PubKey -> Address a
deriveAddress pub = Address (deriveHash pstr)
  where
    (x, y) = Key.extractPoint pub
    pstr   = toS (show x) <> toS (show y)

-- | Address derivation function, maps a hash of a EC point to a unique,
-- irreversible identity that uniquely defines a participant in the network and
-- any participant can verify integrity of it's coherence to a public key.
--
-- > addrHash(n) = sha256(sha256(ripemd160(sha256(n))))
deriveHash :: ByteString -> Hash.Hash Encoding.Base58ByteString
deriveHash = Hash.sha256 . Hash.sha256Raw . Hash.ripemd160Raw . Hash.sha256Raw

parseAddress :: ByteString -> Either Text (Address a)
parseAddress bs =
  case Hash.parseRawHash bs of
    Left err -> Left $ toS $ show err
    Right h  -> Right (Address h)

-- | Verify an address is derived from a given public key.
verifyAddress :: Key.PubKey -> Address a -> Bool
verifyAddress pub a = deriveAddress pub == a

recoverAddress
  :: Encoding.Base64PByteString
  -> ByteString
  -> Either Key.InvalidSignature (Address a, Address a)
recoverAddress sig msg = flip recoverAddress' msg <$> Key.decodeSig sig

recoverAddress' :: Key.Signature -> ByteString -> (Address a, Address a)
recoverAddress' sig = bimap deriveAddress deriveAddress . Key.recover sig

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

-- | Generate a new random 'Address' from random key.
newAddr :: MonadRandom m => m (Address a)
newAddr = Key.new >>= \(pub, priv) -> pure (deriveAddress pub)

-- | Generate a key public key, address pair.
newPair :: MonadRandom m => m (Key.PubKey, Address a)
newPair = Key.new >>= \(pub, priv) -> pure (pub, deriveAddress pub)

-- | Generate a key private key, public key, address pair.
newTriple :: MonadRandom m => m (Key.PrivateKey, Key.PubKey, Address a)
newTriple = Key.new >>= \(pub, priv) -> pure (priv, pub, deriveAddress pub)

-- | Generate a set of new addresses
newAddrs :: MonadRandom m => Int -> m [Address a]
newAddrs n = replicateM n newAddr

-------------------------------------------------------------------------------
-- Postgres DB
-------------------------------------------------------------------------------

-- XXX Maybe this is wrong (trying to convert it to `Text` before toField).
instance ToField (Address a) where
  toField = toField . decodeUtf8 . rawAddr

instance FromField (Address AAccount) where
  fromField = addressFromField

instance FromField (Address AContract) where
  fromField = addressFromField

instance FromField (Address AAsset) where
  fromField = addressFromField

addressFromField
  :: forall k (a :: k). (Typeable k, Typeable a)
  => Field -> Maybe ByteString -> Conversion (Address a)
addressFromField f mdata =
    case mdata of
      Nothing   -> returnError UnexpectedNull f ""
      Just addr -> return $ fromRaw addr

instance ToRow (Address a)
instance FromRow (Address a)
