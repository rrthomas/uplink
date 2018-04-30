{-|

ByteString types with serializers that consume finite, bounded data.

-}

{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module SafeString (
  SafeString,
  maxSize,

  -- ** Conversion
  fromBytes,
  fromBytes',
  toBytes,

  -- ** Serialization
  decodeFixed,
  putSafeString,
  getSafeString,
  encodeSafeString,

  -- ** Testing
  testSafeString,
) where

import GHC.Show (Show(..), show)
import Protolude hiding (Show, show, put, get)

import Control.Monad (fail)
import Control.Exception (throw)

import Crypto.Number.Serialize (os2ip, i2ospOf_)

import Data.Serialize as S
import Data.ByteString.Char8 as B
import Data.Aeson hiding (encode)
import Data.Aeson.Types (typeMismatch)
import qualified Hash
import qualified Data.Aeson as A
import qualified Data.Binary as BI

import Script.Pretty (Pretty(..))

import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField

-- | Maximum number of bytes to read from wire for this field.
maxSize :: Int
maxSize = 10000

-- | Strings safe for network serialization
newtype SafeString = SafeString ByteString
  deriving (Read, Eq, Ord, IsString, Generic, NFData, BI.Binary, Hashable, ToField, FromField)

-- Exceptions for handling invalid string construction
data HugeString = HugeString
  deriving (Eq, Show, Typeable, Generic)

instance Exception HugeString
instance Serialize HugeString

instance Hash.Hashable SafeString where
  toHash (SafeString h) = Hash.toHash h

instance Show SafeString where
  show (SafeString x) = show x
  showsPrec _ (SafeString x) = (++) (show x)

instance Monoid SafeString where
  mempty = SafeString mempty
  mappend (SafeString a) (SafeString b) = SafeString (a<>b)

instance Pretty SafeString where
  ppr (SafeString bs) = ppr (toS bs :: Text)

fromBytes :: ByteString -> Either HugeString SafeString
fromBytes bs
  | len <= maxSize = Right $ SafeString bs
  | otherwise      = Left HugeString
  where
    len = B.length bs

fromBytes' :: ByteString -> SafeString
fromBytes' bs =
  case fromBytes bs of
    Left err -> throw err
    Right ss -> ss

toBytes :: SafeString -> ByteString
toBytes (SafeString s) = s

instance ToJSON SafeString where
  toJSON = toJSON . decodeUtf8 . toBytes

instance FromJSON SafeString where
  parseJSON (String str) =
    case fromBytes (encodeUtf8 str) of
      Left err -> fail $ show err
      Right ss -> pure ss
  parseJSON invalid = typeMismatch "SafeString" invalid

{- Serialization Note:

   Binary encoding of safe strings tightly reflects the binary encoding of the
   (r,s) signature values that are serialized and sent from the SDKs. Modifying
   this code (putSafeString or getSafeString) may cause deserialization of signatures
   sent from the SDKs to fail.

-}

instance Serialize SafeString where
  put = putSafeString

  get = getSafeString

putSafeString :: SafeString -> PutM ()
putSafeString (SafeString bs)
  | len <= maxSize = do
    S.putByteString (i2ospOf_ 2 $ fromIntegral len)
    S.putByteString bs
  | otherwise = throw HugeString
  where
    len :: Int
    len = B.length bs

getSafeString :: Get SafeString
getSafeString = do
  len <- fromIntegral . os2ip <$> getByteString 2
  if len <= maxSize
    then getByteString len >>= pure . SafeString
    else fail "Cannot parse, string is too large."

encodeSafeString :: SafeString -> ByteString
encodeSafeString = S.encode

{-safeDecode :: ByteString -> IO (Either HugeString SafeString)-}
{-safeDecode bs = try (decode bs)-}

testSafeString :: IO ()
testSafeString = do
  test1 <- try $ (pure $ encode (SafeString (B.replicate 999 'a')))
  test2 <- try $ (pure $ encode (SafeString (B.replicate 1000 'a')))
  print (test1 :: Either HugeString ByteString)
  print (test2 :: Either HugeString ByteString)


-- | Decode a JSON blob of a bounded size.
decodeFixed :: A.FromJSON a => ByteString -> Either [Char] a
decodeFixed input =
  if B.length input < maxSize
    then A.eitherDecodeStrict input
    else throw HugeString
