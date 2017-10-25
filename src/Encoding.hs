{-|

Byte encoding schemes.

-}

module Encoding (
  -- ** Base58 Encoding
  b58,
  unb58,
  b58hash,

  -- ** Base16 Encoding
  base16,

  -- ** Base64 Encoding
  base64,
  unbase64,
  base64P,
  unbase64P,
) where

import Protolude
import qualified Hash
import qualified Data.ByteArray as B
import qualified Data.ByteArray.Encoding as B
import qualified Data.ByteString.Base58 as B58

-------------------------------------------------------------------------------
-- Base58 Encoding
-------------------------------------------------------------------------------

-- | Base-58 encode a bytestring.
-- Uses the bitcoin alphabet.
--
-- > 123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz
b58 :: ByteString -> ByteString
b58 = B58.encodeBase58 B58.bitcoinAlphabet

-- | Base-58 decode a bytestring.
unb58 :: ByteString -> Maybe ByteString
unb58 = B58.decodeBase58 B58.bitcoinAlphabet

b58hash :: ByteString -> ByteString
b58hash = b58 . Hash.sha256Raw

-------------------------------------------------------------------------------
-- Base 16
-------------------------------------------------------------------------------

-- | Base 16 encode hash
base16 :: ByteString -> ByteString
base16 = B.convertToBase B.Base16

base64G :: B.Base -> ByteString -> ByteString
base64G = B.convertToBase

unbase64G :: B.Base -> ByteString -> Either [Char] ByteString
unbase64G = B.convertFromBase

-------------------------------------------------------------------------------
-- Base 64
-------------------------------------------------------------------------------

-- | Base 64 encode hash
base64 :: ByteString -> ByteString
base64 = base64G B.Base64URLUnpadded

-- | Base 64 encode hash
unbase64 :: ByteString -> Either [Char] ByteString
unbase64 = unbase64G B.Base64URLUnpadded

-- | Base 64 encode hash (padded)
base64P :: ByteString -> ByteString
base64P = base64G B.Base64

-- | Base 64 encode hash (padded)
unbase64P :: ByteString -> Either [Char] ByteString
unbase64P = unbase64G B.Base64
