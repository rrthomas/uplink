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

import Asset (Asset)
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

-- | Derive the address of a new asset and digitally sign it to produce it's deployment address
addrAsset :: Key.Signature -> Time.Timestamp -> Address -> Address
addrAsset sig issuedOn issuer = Address.fromRaw (Encoding.b58 hash)
  where
    hash    = Hash.getHash (Hash.toHash (r,s,b,c))
    (r,s)   = Key.getSignatureRS sig :: (Integer, Integer)
    b       = issuedOn               :: Time.Timestamp
    c       = issuer                 :: Address

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
