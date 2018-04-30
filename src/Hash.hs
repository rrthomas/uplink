{-|

Hashing for bytestrings and data structures.

-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hash (
  -- ** Types
  Hash,
  getHash,
  getRawHash,
  rawHash,
  hashSize,
  emptyHash,
  isHashOf,

  -- ** Validation Functions
  validateShaBS,

  -- ** Parse Functions
  parseHash,
  parseRawHash,

  -- ** Hashable classes
  Hashable(toHash),

  -- ** Hash Functions (base 16)
  sha256,
  ripemd160,

  -- ** Hash Functions (raw)
  sha256Raw,
  ripemd160Raw,
) where

import Protolude hiding (show, Hashable, hash)
import Prelude (Show(..), Read(..))
import Unsafe (unsafeFromJust)

import GHC.Generics ((:+:)(..), (:*:)(..))
import Crypto.Hash (Digest, SHA3_256, RIPEMD160, hash, digestFromByteString)
import Control.Monad (fail)

import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Aeson.Types (Parser, typeMismatch)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.ByteArray as B
import qualified Data.Binary as B
import qualified Data.Serialize as S
import qualified Data.ByteString as BS
import qualified Data.ByteArray.Encoding as B
import Database.PostgreSQL.Simple.ToField   (ToField(..))
import Database.PostgreSQL.Simple.FromField (FromField(..))

import qualified Encoding

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Size of SHA256 output.
hashSize :: Int
hashSize = 32

-- | Newtype wrapper for a SHA256 digest. Can be converted to a bytestring
-- using 'Data.ByteArray.convert'.
newtype Hash a = Hash { rawHash :: Digest SHA3_256 }
  deriving (Eq, Ord, B.ByteArrayAccess, NFData, Generic)

instance Encoding.ByteStringEncoding a => Show (Hash a) where
  show = show . getRawHash

instance (Encoding.ByteStringEncoding a, Show a) => Read (Hash a) where
  readsPrec i s =
    case readsPrec i s of
      [(bs :: ByteString, "")] ->
        case parseRawHash bs of
          Left err -> fail "Failed to decode Hash Digest"
          Right h  -> pure (h,"")
      otherwise -> fail "Failed to decode Hash Digest"

instance S.Serialize (Hash a) where
  put = S.putByteString . B.convert . rawHash
  get = do
    bs <- S.getByteString hashSize
    case digestFromByteString bs of
      Nothing -> fail "Failed to decode Hash Digest"
      Just d  -> pure $ Hash d

instance (B.Binary a, S.Serialize a, Encoding.ByteStringEncoding a) => B.Binary (Hash a) where
  put h = B.put $ S.encode h
  get = do
    bs <- B.get
    case S.decode bs of
      (Right h) -> return h
      (Left err) -> fail err

instance (Encoding.ByteStringEncoding a) => ToJSON (Hash a) where
  toJSON = toJSON . decodeUtf8 . getRawHash

instance (Encoding.ByteStringEncoding a) => FromJSON (Hash a) where
  parseJSON v = do
    bs <- encodeUtf8 <$> parseJSON v :: Parser ByteString
    case Encoding.parseEncodedBS bs of
      Left err -> typeMismatch "Hash a" v
      Right (encBS :: a) ->
        case digestFromByteString (Encoding.decodeBase encBS) of
          Nothing -> typeMismatch "Hash a" v
          Just  d -> pure $ Hash d

emptyHash :: Hash b
emptyHash = Hash digest
  where
    digest :: Digest SHA3_256
    digest = unsafeFromJust $ digestFromByteString (BS.replicate hashSize 0)

getHash :: (Encoding.ByteStringEncoding a) => Hash a -> a
getHash = Encoding.encodeBase . B.convert . rawHash

getRawHash :: (Encoding.ByteStringEncoding a) => Hash a -> ByteString
getRawHash = Encoding.unbase . getHash

encodeHash :: Hash a -> Hash b
encodeHash (Hash d) = Hash d

-- | SHA256 version of Hashable, customizable on the underlying bytestring
-- conversion. Can be derived using Generics and -XDeriveAnyClass
class Show a => Hashable a where
  -- | SHA hash
  toHash :: Encoding.ByteStringEncoding b => a -> Hash b

  -- | SHA hash ( generic deriving )
  default toHash :: (Generic a, GHashable' (Rep a), Encoding.ByteStringEncoding b) => a -> Hash b
  toHash a = gtoHash (from a)

  -- | Covert to string
  toBS :: a -> ByteString
  toBS a = prefix a <> showHash a

  -- | Prefix the hash input with custom data to distinguish unique types
  prefix :: a -> ByteString
  prefix = const mempty

instance Hashable Int where
  toHash = Hash . hash . showHash

instance Hashable Int64 where
  toHash = Hash . hash . showHash

instance Hashable Float where
  toHash = Hash . hash . showHash

instance Hashable Double where
  toHash = Hash . hash . showHash

instance Hashable Bool where
  toHash = Hash . hash . showHash

instance Hashable Integer where
  toHash = Hash . hash . showHash

instance Hashable ByteString where
  toHash = Hash . hash
  toBS = identity

instance Hashable Encoding.Base16ByteString where
  toHash = toHash . Encoding.unbase16

instance Hashable Encoding.Base58ByteString where
  toHash = toHash . Encoding.unbase58

instance Hashable Encoding.Base64PByteString where
  toHash = toHash . Encoding.unbase64P

instance Hashable Text where
  toHash = Hash . hash . encodeUtf8
  toBS = encodeUtf8

instance Hashable () where
  toHash () = emptyHash

instance Hashable a => Hashable (Maybe a) where
  toHash (Just x) = toHash x
  toHash Nothing = emptyHash

instance (Hashable a, Hashable b) => Hashable (Either a b) where
  toHash (Left x) = toHash x
  toHash (Right x) = toHash x

instance (Hashable a, Hashable b) => Hashable (a,b)
instance (Hashable a, Hashable b, Hashable c) => Hashable (a,b,c)
instance (Hashable a, Hashable b, Hashable c, Hashable d) => Hashable (a,b,c,d)

instance (Encoding.ByteStringEncoding a, Hashable a) => Hashable (Hash a) where
  toHash (Hash d) = toHash (B.convert d :: ByteString)

-- Hash([x0,x1,...xn]) = Hash(x0 | x1 | ... | xn)
--
-- Note:
-- Singleton lists (i.e. [1]) hashes to the same as singleton value (i.e. 1)
--
-- > toHash [0]
-- "5feceb66ffc86f38d952786c6d696c79c2dbc239dd4e91b46729d73a27fb57e9"
-- > toHash 0
-- "5feceb66ffc86f38d952786c6d696c79c2dbc239dd4e91b46729d73a27fb57e9"
instance (Hashable a) => Hashable [a] where
  toBS xs = BS.concat (fmap toBS xs)

instance (Hashable a, Hashable b) => Hashable (Map a b) where
   toHash = toHash . Map.toList

instance Hashable a => Hashable (Set.Set a) where
  toHash = toHash . Set.toList

-------------------------------------------------------------------------------
-- Generic Deriving
-------------------------------------------------------------------------------

-- | Generic Hashable deriving
class GHashable a where

class GHashable' f where
  gtoHash :: Encoding.ByteStringEncoding b => f a -> Hash b
  gtoBS :: f a -> ByteString

instance GHashable' U1 where
  gtoHash _ = emptyHash
  gtoBS _ = ""

instance (Hashable c) => GHashable' (K1 i c) where
  gtoHash (K1 a) = toHash a
  gtoBS (K1 a) = prefix a <> toBS a

instance (GHashable' a) => GHashable' (M1 i c a) where
  gtoHash (M1 a) = gtoHash a
  gtoBS (M1 a) = gtoBS a

instance (GHashable' a, GHashable' b) => GHashable' (a :+: b) where
  gtoHash (L1 a) = gtoHash a
  gtoHash (R1 a) = gtoHash a

  gtoBS (L1 a) = gtoBS a
  gtoBS (R1 a) = gtoBS a

instance (GHashable' a, GHashable' b) => GHashable' (a :*: b) where
  gtoHash (a :*: b) = toHash (gtoBS a <> gtoBS b)
  gtoBS (a :*: b) = gtoBS a <> gtoBS b

-- | Use Haskell's Show representation (converted to ByteString) for the
-- contents of the SHA hash.
showHash :: Show a => a -> ByteString
showHash = toS . show

-- | Check if a given hash is the hashed form a of a given Hashable instance.
isHashOf :: Hashable a => a -> Hash ByteString -> Bool
isHashOf x hsh = toHash x == hsh

-------------------------------------------------------------------------------
-- Validation
-------------------------------------------------------------------------------

-- | Validate a bytestring as a valid base16 encoded hash.
-- TODO: Hash this value
validateShaBS :: Encoding.ByteStringEncoding a => a -> Bool
validateShaBS base =
  compareHashSize (Encoding.decodeBase base) hashSize

compareHashSize :: ByteString -> Int -> Bool
compareHashSize bs hashSize = B.length bs == hashSize

-------------------------------------------------------------------------------
-- Parsing hashes
-------------------------------------------------------------------------------

-- | Convert Bytestring to hash values when applicable
parseHash
  :: Encoding.ByteStringEncoding b
  => b -> Either Text (Hash.Hash a)
parseHash encBS =
  if validateShaBS encBS
     then case digestFromByteString decBS of
            Nothing -> Left (toS decBS <> " is an invalid hash")
            Just d  -> Right $ Hash d
     else Left (toS decBS <> " is an invalid hash")
  where
    decBS = Encoding.decodeBase encBS

parseRawHash
  :: forall a. (Show a, Encoding.ByteStringEncoding a)
  => ByteString -> Either Text (Hash.Hash a)
parseRawHash bs =
  case Encoding.parseEncodedBS bs of
    Left err    -> Left (toS $ show err)
    Right (encBS :: a) -> parseHash encBS

-------------------------------------------------------------------------------
-- Hash Functions
-------------------------------------------------------------------------------

-- | Compute SHA-256 hash of a bytestring.
-- Maximum input size is (2^{64}-1)/8 bytes.
--
-- > Output size         : 256
-- > Internal state size : 1600
-- > Block size          : 1088
-- > Length size         : n/a
-- > Word size           : 64
-- > Rounds              : 24
sha256 :: Encoding.ByteStringEncoding a => ByteString -> Hash a
sha256 x = toHash x

sha256Raw :: ByteString -> ByteString
sha256Raw x = getHash $ sha256 x

-- | Compute RIPEMD-160 hash of a bytestring.
--
-- > Output size         : 160
-- > Internal state size : 128
-- > Block size          : 512
-- > Length size         : 64
-- > Word size           : 32
-- > Rounds              : 80
ripemd160 :: ByteString -> ByteString
ripemd160 x = B.convertToBase B.Base16 (hash x :: Digest RIPEMD160)

ripemd160Raw :: ByteString -> ByteString
ripemd160Raw x = B.convert (hash x :: Digest RIPEMD160)

-------------------------------------------------------------------------------
-- PostgreSQL
-------------------------------------------------------------------------------

instance ToField (Hash Encoding.Base16ByteString) where
  toField = toField . getHash

instance FromField (Hash Encoding.Base16ByteString) where
  fromField f mdata = do
    b16bs :: Encoding.Base16ByteString <- fromField f mdata
    let unencBS = Encoding.decodeBase b16bs
    case digestFromByteString unencBS of
      Nothing -> fail "Failed to decode base16 encoded Hash Digest"
      Just d  -> pure $ Hash d

instance ToField (Hash Encoding.Base58ByteString) where
  toField = toField . getHash

instance FromField (Hash Encoding.Base58ByteString) where
  fromField f mdata = do
    b16bs :: Encoding.Base58ByteString <- fromField f mdata
    let unencBS = Encoding.decodeBase b16bs
    case digestFromByteString unencBS of
      Nothing -> fail "Failed to decode base58 encoded Hash Digest"
      Just d  -> pure $ Hash d

-------------------------------------------------------------------------------
-- Testing
-------------------------------------------------------------------------------

data TestHash = TestHash Int ByteString
  deriving (Show, Generic)

-- Can be derived with DeriveAnyClass but doesn't play nice with GND. For now,
-- wait until GHC 8.2
instance Hashable TestHash
