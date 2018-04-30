{-|

Threshold signatures.

-}

{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Threshold (
  -- ** Generation
  new,
  combine,
) where

import Protolude

import qualified Key
import qualified Shamir
import qualified Encoding

-- | Create a new secret escrow.
-- t: The number of shares needed to reconstitute the secret
-- n: The total number of shares.
new :: Word8 -> Word8 -> IO (Key.PubKey, Shamir.Secrets)
new t n = do
  (pk, sk) <- Key.new
  shares <- Shamir.split t n (Key.exportPriv sk)
  return (pk, shares)

-- | Combine secrets to reconsitute a message.
combine :: Shamir.Secrets -> Encoding.Base64ByteString
combine = Encoding.encodeBase . Shamir.combine
