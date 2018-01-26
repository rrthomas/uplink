{-|

GMP integer types with serializers that consume finite, bounded data.

-}

{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fwarn-name-shadowing #-}

module SafeInteger (
  SafeInteger,
  HugeInteger, -- Exception
  maxBits,
  maxBytes,

  toSafeInteger,
  toSafeInteger',
  fromSafeInteger,

  unsafeToSafeInteger,
  unsafeEncode,

  roll,
  unroll
) where

import qualified Prelude (show)
import Protolude hiding (put, get)

import Control.Monad (fail)
import Control.Exception (Exception, throw)

import Crypto.Number.Basic (numBits, numBytes)

import Data.Aeson as A
import Data.Aeson.Types as A
import Data.Hashable (Hashable)
import Data.Serialize as S
  (Serialize(..), runPut, Get
  , getWord8, putWord8
  , getWord16be, putWord16be
  , encode, decode
  )
import Data.Text.Read (signed, decimal)

import Script.Pretty
import qualified Hash

import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField

maxBits :: Int
maxBits = 4096

maxBytes :: Int
maxBytes = extra + maxBits `div` 8
  where
    extra
      | maxBits `rem` 8 == 0 = 0
      | otherwise = 1

maxBound' :: Integer
maxBound' = 2^maxBits - 1

minBound' :: Integer
minBound' = -1 * maxBound'

data HugeInteger = HugeInteger
  deriving (Eq, Show)

instance Exception HugeInteger

-- | Integers safe for serialization
newtype SafeInteger = SafeInteger Integer
  deriving (Eq, Ord, Read, Hashable, NFData)

instance Bounded SafeInteger where
  maxBound = SafeInteger maxBound'
  minBound = SafeInteger minBound'

instance Hash.Hashable SafeInteger where
  toHash (SafeInteger n) = Hash.toHash n

instance Show SafeInteger where
  show (SafeInteger x) = show x

instance Pretty SafeInteger where
  ppr (SafeInteger x) = ppr x

instance ToField SafeInteger where
  toField (SafeInteger si) = toField si

instance FromField SafeInteger where
  fromField f mdata = SafeInteger <$> fromField f mdata

instance ToField (SafeInteger, SafeInteger) where
  toField = EscapeByteA . S.encode

instance FromField (SafeInteger, SafeInteger) where
  fromField f mdata = do
    bs <- fromField f mdata
    case S.decode <$> bs of
      Nothing             -> returnError UnexpectedNull f ""
      Just (Left err)     -> returnError ConversionFailed f (toS err)
      Just (Right twoSIs) -> return twoSIs

-- | Must encode as String because JSON/Javascript only supports up to 64 bit
-- integers.
instance ToJSON SafeInteger where
  toJSON (SafeInteger n) = A.String $ show n

instance FromJSON SafeInteger where
  parseJSON (String s) = case signed decimal s of
    Left err -> typeMismatch err (String s)
    Right (n,_) -> case toSafeInteger n of
      Left err -> typeMismatch "SafeInteger" (String s)
      Right sn -> pure sn
  parseJSON invalid = typeMismatch "SafeInteger" invalid

fromSafeInteger :: SafeInteger -> Integer
fromSafeInteger (SafeInteger n) = n

-- | Converts an Integral type to a SafeInteger
toSafeInteger :: Integral a => a -> Either HugeInteger SafeInteger
toSafeInteger n
  | numBits nInteger <= maxBits = Right $ SafeInteger nInteger
  | otherwise                   = Left HugeInteger
  where
    nInteger = toInteger n

-- | Unsafe version of `toSafeInteger`, will throw exception if the value being
-- converted is larger than `maxBits`
toSafeInteger' :: Integral a => a -> SafeInteger
toSafeInteger' n
  | numBits nInteger <= maxBits = SafeInteger nInteger
  | otherwise                   = throw HugeInteger
  where
    nInteger = toInteger n

unsafeToSafeInteger :: Integral a => a -> SafeInteger
unsafeToSafeInteger n = SafeInteger $ toInteger n

instance Serialize SafeInteger where
  put (SafeInteger n)
    | len <= maxBytes = do
        putWord8 sign
        putWord16be $ fromIntegral len
        mapM_ putWord8 bytes
    | otherwise = throw HugeInteger
    where
      sign = fromIntegral (signum n) :: Word8
      bytes = unroll (abs n)
      len  = length bytes

  get = do
    sign <- getWord8
    len' <- getWord16be
    let len = fromIntegral len'
    bytes <- replicateM len getWord8
    if len <= maxBytes
      then do
        let v = roll bytes
        pure $! SafeInteger $
          if sign == (1 :: Word8) then v else (-v)
      else throw HugeInteger

-- | Encode a SafeInteger using binary serialization to a strict ByteString,
-- ignoring the `maxBytes` limit to the size of a SafeInteger
unsafeEncode :: SafeInteger -> ByteString
unsafeEncode (SafeInteger n) = runPut $ do
    putWord8 sign
    putWord8 leadingZeros
    forM_ paddedBytes putWord8
  where
    sign = fromIntegral (signum n) :: Word8

    bytes = unroll (abs n)
    len  = length bytes
    pad = replicate (maxBytes - len) 0
    paddedBytes = pad ++ bytes

    leadingZeros = fromIntegral $
      length $ takeWhile (==0) bytes

-- | Unfold an Integral type to list of its bytes
unroll :: (Integral a, Bits a) => a -> [Word8]
unroll = unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)

-- | Fold a list of bytes into an Integral type
roll :: (Integral a, Bits a) => [Word8] -> a
roll = foldr unstep 0
  where
    unstep a b = b `shiftL` 8 .|. fromIntegral a
