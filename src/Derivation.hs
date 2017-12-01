{-|

Address derivation functions.

-}

module Derivation (
  addrAsset,
  addrAccount,
  addrContract,
  addrContract',
) where

import Protolude

import qualified Data.Serialize as S

import Asset (Asset(..))
import Account (Account)
import Address (Address)
import Contract (Contract(..))

import qualified Key
import qualified Hash
import qualified Time
import qualified Asset
import qualified Address
import qualified Account
import qualified Storage
import qualified Contract
import qualified Encoding

-- | XXX Currently, this address derivation does not match the Python SDK's
-- derivation of asset addresses. We should sync them soon, as the
-- `Validate.applyTxAsset CreateAsset{..}` case should throw err on diff addr

addrAsset
  :: ByteString      -- ^ Asset Name
  -> Address         -- ^ Asset issuer
  -> Asset.Balance   -- ^ Asset Supply
  -> Maybe Asset.Ref -- ^ Reference
  -> Asset.AssetType -- ^ Type of Asset Supply
  -> Time.Timestamp  -- ^ Timestamp of transaction issuing asset
  -> Address
addrAsset name issuer supply mref typ txts =
    Address.fromRaw (Encoding.b58 hash)
  where
    hash = Hash.getHash $ Hash.toHash $
      mconcat [ n, i, s, r, t, ts ]

    n = name
    i = S.encode issuer
    s = S.encode supply
    r = S.encode mref
    t = S.encode typ
    ts = S.encode txts

-- | Derive the address of an contract
addrContract :: Contract -> Address
addrContract Contract{..} = addrContract' timestamp globalStorage

-- | XXX Currently, this address derivation does not match the Python SDK's
-- derivation of contract addresses. We should sync them in the near future
--
-- SDK Address derivation:
--   b58(sha3_256(str(timestamp) + rawScriptText))
addrContract' :: Time.Timestamp -> Storage.GlobalStorage -> Address
addrContract' ts gstore = Address.fromRaw (Encoding.b58 hash)
  where
    hash        = Hash.getHash (Hash.toHash payload)
    payload     = (ts, storageHash)
    storageHash = Storage.hashGlobalStorage gstore

-- | Derive the address of an account
addrAccount :: Account -> Address
addrAccount = Account.address
