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

module Address (
  -- ** Types
  Address,
  rawAddr,
  fromRaw,
  emptyAddr,
  isEmpty,
  shortAddr,
  addrSize,

  -- ** Tagging
  AContract,
  AAccount,
  AAsset,
  showAddr,

  -- ** Parsing
  parseAddr,
  parseAddress,

  -- ** Validation
  deriveHash,
  deriveAddress,
  verifyAddress,
  validateAddress,
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
import Protolude hiding (Show, show)
import qualified GHC.TypeLits as Lits

import Encoding
import qualified Key
import qualified Hash
import Script.Pretty (Pretty(..), squotes)

import Control.Monad (fail)

import Data.Aeson (Value(..), ToJSON(..), FromJSON(..), ToJSONKey(..), FromJSONKey(..), FromJSONKeyFunction(..))

import Data.Aeson.Types (typeMismatch, toJSONKeyText)
import Data.Hashable (Hashable)
import Data.Serialize (Serialize, encode, decode)
import qualified Data.Binary as B
import qualified Data.Serialize as S
import qualified Data.ByteString as BS

import Database.PostgreSQL.Simple.FromRow   (FromRow)
import Database.PostgreSQL.Simple.ToRow     (ToRow(..))
import Database.PostgreSQL.Simple.ToField   (ToField(..))
import Database.PostgreSQL.Simple.FromField (FromField(..), ResultError(..), returnError)

import Crypto.Number.Serialize (i2osp)

-------------------------------------------------------------------------------
-- Address
-------------------------------------------------------------------------------

-- | Size of a base58 encoded bytestring address
addrSize :: Int
addrSize = Hash.hashSize

-- | XXX: Roll underlying type to use Digest SHA3_256
-- | XXX: phantom type parameter to distinguish address types

-- | A ledger address, derived from elliptic curve point
newtype Address = Address ByteString
  deriving (Eq, Ord, Monoid, Generic, NFData, B.Binary, Hashable, Hash.Hashable, Read)

instance ToJSON Address where
  toJSON (Address bs) = Data.Aeson.String (decodeUtf8 bs)

instance FromJSON Address where
  parseJSON (String v) =
    case unb58 (encodeUtf8 v) of
      Nothing -> fail "String is not valid base 58 encoded."
      Just bs ->
        case validateAddress (Address (encodeUtf8 v)) of
          False -> fail "String is malformed address."
          True  -> pure (Address (encodeUtf8 v))
  parseJSON _ = fail "Cannot parse address from non-string."

instance FromJSONKey Address where
  fromJSONKey = Address . encodeUtf8 <$> fromJSONKey

instance Serialize Address where
  put (Address bs) = case unb58 bs of
    Just raw -> S.putByteString raw
    Nothing  -> panic ("Cannot serialize invalid address:" <> (toS (show bs)))
  get = Address . b58 <$> S.getByteString addrSize

instance Pretty Address where
  ppr (Address bs) = squotes $ ppr bs


instance ToJSONKey Address where
  toJSONKey = toJSONKeyText (decodeUtf8 . rawAddr)

-- | Type level tags of address type
data AContract
data AAccount
data AAsset

type family AddrTag i where
  AddrTag AContract = "c"
  AddrTag AAccount  = "u"
  AddrTag AAsset    = "a"
  AddrTag _         = Lits.TypeError (Lits.Text "Cannot lower tag of unknown address")

-- | Lower the typelevel tag of an address to it's display value.
{-showAddr :: forall a. (KnownSymbol (AddrTag a)) => (Address a) -> Text-}
showAddr :: forall a. (KnownSymbol (AddrTag a)) => (a, Address) -> Text
showAddr (_, addr) = toS (symbolVal (Proxy :: Proxy (AddrTag a))) <> toS (show addr)

putAddress :: Address.Address -> S.PutM ()
putAddress addr =
  case unb58 $ rawAddr addr of
    Nothing -> fail "Invalid base58 encoded address."
    Just addr' -> S.putByteString addr'

getAddress :: S.Get Address.Address
getAddress =  parseAddress . b58 <$> S.getByteString addrSize

-- | Extract underlying bytes from an 'Address'
rawAddr :: Address -> ByteString
rawAddr (Address n) = n

-- | Build an 'Address' from a bytestring. ( Not safe )
fromRaw :: ByteString -> Address
fromRaw bs =
  let addr = Address bs
  in if validateAddress addr
    then addr
    else panic $ "Cannot validate address as input to \'fromRaw\': " <> toS bs

-- | Empty address
emptyAddr :: Address
emptyAddr = Address (b58 (BS.replicate addrSize 0))

-- | Check if address is empty
isEmpty :: Address -> Bool
isEmpty (Address s) = s == mempty

-- | Shortened address for logging
shortAddr :: Address -> ByteString
shortAddr (Address addr) = BS.take 7 addr

-- | Derive an address from a public key
--
-- > address(x,y) = addrHash(string(x) <> string(y))
deriveAddress :: Key.PubKey -> Address
deriveAddress pub = Address (b58 addr)
  where
    (x, y) = Key.extractPoint pub
    addr   = deriveHash pstr
    pstr   = toS (show x) <> toS (show y)

-- | Address derivation function, maps a hash of a EC point to a unique,
-- irreversible identity that uniquely defines a participant in the network and
-- any participant can verify integrity of it's coherence to a public key.
--
-- > addrHash(n) = sha256(sha256(ripemd160(sha256(n))))
deriveHash :: ByteString -> ByteString
deriveHash = Hash.sha256Raw . Hash.sha256Raw . Hash.ripemd160Raw . Hash.sha256Raw

-- | Validate whether an address is a well-formed B58 encoded hash.
validateAddress :: Address -> Bool
validateAddress (Address s) = case unb58 s of
  Nothing  -> False
  Just sha -> Hash.validateSha sha

-- | Parses a ByteStrig into an Address. Panics on invalid inputs.
parseAddress :: ByteString -> Address
parseAddress = fromRaw

-- | XXX why do we have two of these
parseAddr :: ByteString -> Maybe Address
parseAddr bs =
  let addr = Address bs
  in if validateAddress addr
    then Just addr
    else Nothing

-- | Verify an address is derived from a given public key.
verifyAddress :: Key.PubKey -> Address -> Bool
verifyAddress pub a@(Address s) = case unb58 s of
  Nothing -> False
  Just _  -> deriveAddress pub == a

recoverAddress :: ByteString -> ByteString -> Either Key.InvalidSignature (Address,Address)
recoverAddress sig msg = flip recoverAddress' msg <$> Key.decodeSig sig

recoverAddress' :: Key.Signature -> ByteString -> (Address,Address)
recoverAddress' sig = bimap deriveAddress deriveAddress . Key.recover sig

-- Hackish show method for debugging
instance Show Address where
  show (Address "") = "<empty>"
  show (Address x) = toS x

  showsPrec _ (Address "") = (++) "<empty>"
  showsPrec _ (Address x) = (++) (toS x)

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

-- | Generate a new random 'Address' from random key.
newAddr :: IO Address
newAddr = Key.new >>= \(pub, priv) -> pure (deriveAddress pub)

-- | Generate a key public key, address pair.
newPair :: IO (Key.PubKey, Address)
newPair = Key.new >>= \(pub, priv) -> pure (pub, deriveAddress pub)

-- | Generate a key private key, public key, address pair.
newTriple :: IO (Key.PrivateKey, Key.PubKey, Address)
newTriple = Key.new >>= \(pub, priv) -> pure (priv, pub, deriveAddress pub)

-- | Generate a set of new addresses
newAddrs :: Int -> IO [Address]
newAddrs n = replicateM n newAddr

-------------------------------------------------------------------------------
-- Postgres DB
-------------------------------------------------------------------------------

-- XXX Maybe this is wrong (trying to convert it to `Text` before toField).
instance ToField Address where
  toField = toField . decodeUtf8 . rawAddr

instance FromField Address where
  fromField f mdata =
    case mdata of
      Nothing   -> returnError UnexpectedNull f ""
      Just addr
        | validateAddress (fromRaw addr) -> return $ fromRaw addr
        | otherwise -> returnError ConversionFailed f "Invalid Address read from DB"

instance ToRow Address
instance FromRow Address
