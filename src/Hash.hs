{-|

Hashing for bytestrings and data structures.

-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--{-# LANGUAGE DeriveAnyClass #-}
--{-# LANGUAGE DerivingStrategies #-} in GHC 8.2

module Hash (
  -- ** Types
  Hash,
  getHash,
  rawHash,
  hashSize,
  emptyHash,
  isHashOf,
  validateSha,
  validateSha16,

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
import Prelude (Show(..))
import Unsafe (unsafeFromJust)

import GHC.Generics ((:+:)(..), (:*:)(..))
import Crypto.Hash (Digest, SHA3_256, RIPEMD160, hash, digestFromByteString)

import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.ByteArray as B
import qualified Data.Serialize as S
import qualified Data.ByteString as BS
import qualified Data.ByteArray.Encoding as B

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Size of SHA256 output.
hashSize :: Int
hashSize = 32

-- XXX: roll over to use Hash type for all digests. Instead of raw ByteString.
-- XXX: I second this idea!

-- | Newtype wrapper for a SHA256 digest. Can be converted to a bytestring
-- using 'Data.ByteArray.convert'.
newtype Hash a = Hash { rawHash :: Digest SHA3_256 }
  deriving (Eq, Ord, B.ByteArrayAccess)

emptyHash :: Hash ByteString
emptyHash = Hash digest
  where
    digest :: Digest SHA3_256
    digest = unsafeFromJust $ digestFromByteString (BS.replicate hashSize 0)

instance Show (Hash a) where
  show x = show ((B.convertToBase B.Base16 (rawHash x)) :: ByteString)

getHash :: Hash ByteString -> ByteString
getHash x = B.convert (rawHash x)

-- | SHA256 version of Hashable, customizable on the underlying bytestring
-- conversion. Can be derived using Generics and -XDeriveAnyClass
class Show a => Hashable a where
  -- | SHA hash
  toHash :: a -> Hash ByteString

  -- | SHA hash ( generic deriving )
  default toHash :: (Generic a, GHashable' (Rep a)) => a -> Hash ByteString
  toHash a = gtoHash (from a)

  -- | Covert to string
  toBS :: a -> ByteString
  toBS a = (prefix a) <> (showHash a)

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
  gtoHash :: f a -> Hash ByteString
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

-- | Validate a bytestring as a valid base16 encoded hash.
validateSha16 :: ByteString -> Bool
validateSha16 x = case B.convertFromBase B.Base16 x of
  Left err -> False
  Right bs -> B.length (bs :: ByteString) == hashSize

validateSha :: ByteString -> Bool
validateSha bs = B.length (bs :: ByteString) == hashSize

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
sha256 :: ByteString -> ByteString
sha256 x = B.convertToBase B.Base16 (hash x :: Digest SHA3_256)

sha256Raw :: ByteString -> ByteString
sha256Raw x = B.convert (hash x :: Digest SHA3_256)

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
-- Testing
-------------------------------------------------------------------------------

data TestHash = TestHash Int ByteString
  deriving (Show, Generic)

-- Can be derived with DeriveAnyClass but doesn't play nice with GND. For now,
-- wait until GHC 8.2
instance Hashable TestHash
