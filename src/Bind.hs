{-|

Asset and contract binding.

-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Bind (
  BindProof(..),
  checkBind,
) where

import Protolude

import Key
import Asset
import Address
import Account
import Contract

import Data.Serialize as S

-------------------------------------------------------------------------------
-- Asset
-------------------------------------------------------------------------------

-- | A bind proof (signature) is a confirmation that a public asset is held by a
-- counterparty claiming to hold it verified with the public ledger balance
-- state and a signature of the contract signed with the holders private key.
--
-- Bind proofs are held in the state of contract and determine which asset
-- operations emitted during execution are valid ledger transaction.
data BindProof = BindProof
  { assetAddr    :: Address            -- ^ Address of asset to bind
  , contractAddr :: Address            -- ^ Contract address
  , assetProof   :: (Integer, Integer) -- ^ Contract address signed with holder's private key
  } deriving (Generic, Serialize)

checkBind :: Contract -> Account -> Asset -> BindProof -> Bool
checkBind c act asset prf = test
  where
    (r,s) = assetProof prf

    test :: Bool
    test = and [
        Key.verify (publicKey act) (mkSignatureRS (r,s)) (S.encode prf)
      , Account.address act == assetAddr prf
      , Contract.address c == contractAddr prf
      ]
