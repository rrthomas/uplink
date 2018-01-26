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

import qualified Data.Map as Map

-- | Create a new secret escrow.
-- t: The number of shares needed to reconstitute the secret
-- n: The total number of shares.
new :: Word8 -> Word8 -> IO (Key.PubKey, Shamir.Secrets)
new t n = do
  (pk, sk) <- Key.new
  shares <- Shamir.split t n (Key.exportPriv sk)
  return (pk, (fmap Encoding.base64 shares))

-- | Combine secretes to reconsitute a message.
combine :: Shamir.Secrets -> Either [Char] ByteString
combine secrets = do
  secrets' <- mapM Encoding.unbase64 secrets
  pure (Shamir.combine secrets')
