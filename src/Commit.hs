{-|

SyncLocal commitments.

-}

module Commit (
  -- ** Cryptographic ops
  commitTo,
  verifyCommit,

  -- ** SyncLocal transactions
  shareReveal,
  shareFinalize,
) where

import Protolude

import Key
import Storage
import Address
import SafeInteger
import Transaction
import qualified Pedersen

import Crypto.Number.Serialize
import qualified Crypto.PubKey.ECC.Types as ECC

import qualified Data.Serialize as S

-- | Generate commitment for storage state
commitTo
  :: Storage
  -> IO (Pedersen.ECCommitParams, ECC.Point)
commitTo store = do
  let hash = Storage.hashStorage store
  commitParams <- Pedersen.ecSetup Nothing
  (Pedersen.ECPedersen commitment r) <- Pedersen.ecCommit hash commitParams
  pure (commitParams, Pedersen.unECCommitment commitment)

-- | Verify a commitment for storage state
verifyCommit
  :: Pedersen.ECCommitParams
  -> ECC.Point
  -> Pedersen.ECReveal
  -> Bool
verifyCommit params cmt = Pedersen.ecOpen params (Pedersen.ECCommitment cmt)

-- | Publish a finalize commit reveal with another counterparty given by public
-- key address.
shareReveal
  :: (S.Serialize a)
  => Key.PrivateKey
  -> Key.PubKey
  -> a
  -> IO ByteString
shareReveal priv1 pub2 commit = do
  let msecret = Key.dhGetShared priv1 pub2 -- XXX
  case msecret of
    Nothing -> panic "Cannot derive Diffie-Hellman shared secret"
    Just secret -> do
      let sec1 = secretToPrivate secret
      let (Key.SharedKey dhbs) = Key.dhSecret sec1
      encrypt dhbs (S.encode commit)

-- | Construct a finalization SyncLocal transaction.
shareFinalize
  :: Key.PrivateKey
  -> Key.PubKey
  -> Address AContract
  -> Storage
  -> IO (Either HugeInteger SyncLocalOp)
shareFinalize priv pub addr store = do
  let hash = Storage.hashStorage store
  reveal <- shareReveal priv pub hash
  case toSafeInteger (os2ip reveal) of
    Left err -> pure (Left err)
    Right bs -> pure (Right (Finalize addr bs))
