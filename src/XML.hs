{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module XML (
  ToXML(..),
) where

import Protolude

import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree
import Text.XML.Expat.Format
import Unsafe (unsafeFromJust)

import Utils
import Block
import Asset
import Script
import Address
import SafeString
import SafeInteger
import Transaction
import Script.Parser
import Datetime.Types
import Consensus.Authority.Types
import qualified Key
import qualified Data.Set as Set
import qualified Script.Pretty as Pretty
import qualified Datetime.Types as DT

-------------------------------------------------------------------------------
-- Generic Serializer
-------------------------------------------------------------------------------

-- | Convert a generic data structure to structured XML.
class ToXML a where
  toXML :: [a] -> LByteString

instance ToXML Block where
  toXML = renderXML "blocks" xpBlock

instance ToXML BlockHeader where
  toXML = renderXML "blockHeaders" xpBlockHeader

instance ToXML BlockSignature where
  toXML = renderXML "blockSignatures" xpBlockSignature

instance ToXML Transaction where
  toXML = renderXML "transactions" xpTransaction

instance ToXML TransactionHeader where
  toXML = renderXML "transactionHeaders" xpTransactionHeader

renderXML :: Text -> (PU [Node Text Text] a) -> [a] -> LByteString
renderXML label f v = format (indent 4 (pickleTree body v))
  where
    body = xpRoot (xpElemNodes label $ xpList f)

-------------------------------------------------------------------------------
-- Serializers
-------------------------------------------------------------------------------

xpAddress :: PU Text Address
xpAddress =
  xpWrap (fromRaw, rawAddr) xpByteString

xpByteString :: PU Text ByteString
xpByteString =
  xpWrap (encodeUtf8, decodeUtf8) xpText0


xpBlock :: PU [Node Text Text] Block
xpBlock =
   xpWrap (\(index, (signatures, header, transactions)) -> Block header (Set.fromList $signatures) index transactions,
           \(Block header signatures index transactions) -> (index, (Set.toList signatures, header, transactions))) $
   xpElem "block"
           (xpAttr "index" xpPrim)
       (xpTriple
        (xpElemNodes "signatures" (xpList0 xpBlockSignature))
        xpBlockHeader
        (xpElemNodes "transactions" (xpList0 xpTransaction)))

xpBlockHeader :: PU [UNode Text] BlockHeader
xpBlockHeader =
  xpWrap (\((origin, prevHash, merkleRoot, timestamp), consensus) -> BlockHeader origin prevHash merkleRoot timestamp consensus,
    \(BlockHeader origin prevHash merkleRoot timestamp consensus) -> ((origin, prevHash, merkleRoot, timestamp), consensus)) $
  xpElem "header"
    (xp4Tuple
      (xpAttr "origin" xpPrim)
      (xpAttr "prevHash" xpPrim)
      (xpAttr "merkleRoot" xpPrim)
      (xpAttr "timestamp" xpPrim)
      )
    xpConsensus

xpBlockSignature :: PU [UNode Text] BlockSignature
xpBlockSignature =
  xpWrap (\(sig,addr) -> BlockSignature (textToSig sig) addr, \(BlockSignature sig addr) -> (sigToText sig, addr)) $
    xpElemAttrs "BlockSignature" $
      xpPair (xpAttr "signature" xpPrim) (xpAttr "signerAddr" xpAddress)
  where
    sigToText :: Key.Signature -> Text
    sigToText = toS . Key.encodeSig

    textToSig :: Text -> Key.Signature
    textToSig = Key.decodeSig' . toS

xpConsensus :: PU [UNode Text] PoA
xpConsensus =
  xpWrap (\(validatorSet, blockPeriod, blockGenLimit, blockSignLimit, threshold, minTxs) -> PoA (ValidatorSet validatorSet) blockPeriod blockGenLimit blockSignLimit threshold minTxs,
    \(PoA validatorSet blockPeriod blockGenLimit blockSignLimit threshold minTxs) -> (unValidatorSet validatorSet, blockPeriod, blockGenLimit, blockSignLimit, threshold, minTxs)) $
  xpElemAttrs "consensus"
    (xp6Tuple
      (xpAttr "validatorSet" xpPrim)
      (xpAttr "blockPeriod" xpPrim)
      (xpAttr "blockGenLimit" xpPrim)
      (xpAttr "blockSignLimit" xpPrim)
      (xpAttr "threshold" xpPrim)
      (xpAttr "minTxs" xpPrim))

xpTransaction :: PU [Node Text Text] Transaction
xpTransaction =
   xpWrap (\((signature, origin, to, timestamp), header) -> Transaction header (encodeUtf8 signature) origin to timestamp,
           \(Transaction header signature origin to timestamp) -> ((decodeUtf8 signature, origin, to, timestamp), header)) $
   xpElem "transaction"
       (xp4Tuple
           (xpAttr "signature" xpText0)
           (xpAttr "origin" xpPrim)
           (xpAttr "to" xpPrim)
           (xpAttr "timestamp" xpPrim))
       xpTransactionHeader

xpTransactionHeader :: PU [UNode Text] TransactionHeader
xpTransactionHeader =
  xpAlt tag ps
  where
    tag (TxContract _) = 0
    tag (TxAsset _) = 1
    tag (TxAccount _) = 2
    ps = [ xpWrap (TxContract, \(TxContract h) -> h) $ xpTxContract
         , xpWrap (TxAsset, \(TxAsset h) -> h) $ xpTxAsset
         , xpWrap (TxAccount, \(TxAccount h) -> h) $ xpTxAccount
      ]

xpTxContract :: PU [UNode Text] TxContract
xpTxContract =
  xpAlt tag ps
  where
    tag (CreateContract _ _) = 0
    tag (SyncLocal _ _) = 1
    tag (Call  _ _ _) = 2
    ps = [ xpWrap (\(address, contract) -> CreateContract address (SafeString.fromBytes' contract),
                    \(CreateContract address contract) -> (address, SafeString.toBytes contract))
            $ xpElem "CreateContract"
              (xpAttr "address" xpAddress)
              (xpContent xpPrim)
          , xpWrap (\(address, op) -> SyncLocal address op,
            \(SyncLocal address op) -> (address, op))
            $ xpElem "SyncLocal"(xpAttr "address" xpAddress) xpSyncLocal
          , xpWrap (\((address, method), args) -> Call address method args,
                    \(Call address method args) -> ((address, method), args))
            $ xpElem "Call"
              (xpPair
                (xpAttr "address" xpAddress)
                (xpAttr "method" xpByteString))
              (xpList0 xpValue)
      ]

xpSyncLocal :: PU [UNode Text] SyncLocalOp
xpSyncLocal =
  xpAlt tag ps
    where
      tag (InitialCommit _) = 0
      tag (Sync) = 1
      tag (Finalize _ _) = 2
      ps = [ xpWrap (\(commitX, commitY) -> InitialCommit (toSafeInteger' commitX, toSafeInteger' commitY), \(InitialCommit (commitX, commitY)) -> (fromSafeInteger commitX, fromSafeInteger commitY))
          $ xpElemAttrs "InitialCommit"
            (xpPair
              (xpAttr "commitX" xpPrim)
              (xpAttr "commitY" xpPrim))
        ]

xpValue :: PU [UNode Text] Script.Value
xpValue =
  xpAlt tag ps
    where
      tag (VInt _)       = 0
      tag (VCrypto _)    = 1
      tag (VFloat _)     = 2
      tag (VFixed _)     = 3
      tag (VBool _)      = 4
      tag VVoid          = 5
      tag (VSig _)       = 6
      tag (VMsg _)       = 7
      tag (VAddress _)   = 8
      tag (VAccount _)   = 9
      tag (VAsset _)     = 10
      tag (VContract _)  = 11
      tag (VDateTime _)  = 12
      tag (VTimeDelta _) = 13
      tag (VState _)     = 14
      tag VUndefined     = 15
      ps = [ xpWrap (VInt, \(VInt v) -> v)
             $ xpElemNodes "VInt" (xpContent xpPrim)
           , xpWrap (VCrypto . toSafeInteger', \(VCrypto v) -> fromSafeInteger v)
             $ xpElemNodes "VCrypto" (xpContent xpPrim)
           , xpWrap (VFloat, \(VFloat v) -> v)
             $ xpElemNodes "VFloat" (xpContent xpPrim)
           , xpWrap (VFixed . (\(Right v) -> v) . Script.Parser.parseFixedN,  -- XXX
             \(VFixed v) -> Pretty.prettyPrint v) $ xpElemNodes "VFixed" (xpContent xpPrim)
           , xpWrap (VBool, \(VBool v) -> v)
             $ xpElemNodes "VBool" (xpContent xpPrim)
           , xpWrap (\_ -> VVoid, \VVoid -> ())
             $ xpElemNodes "VVoid" xpUnit
           , xpWrap (VSig, \(VSig v) -> v)
             $ xpElemNodes "VSig" (xpContent xpPrim)
           , xpWrap (VMsg, \(VMsg v) -> v)
             $ xpElemNodes "VMsg" (xpContent xpPrim)
           , xpWrap (VAddress, \(VAddress v) -> v)
             $ xpElemNodes "VAddress" (xpContent xpAddress)
           , xpWrap (VAccount, \(VAccount v) -> v)
             $ xpElemNodes "VAccount" (xpContent xpAddress)
           , xpWrap (VAsset, \(VAsset v) -> v)
             $ xpElemNodes "VAsset" (xpContent xpAddress)
           , xpWrap (VContract, \(VContract v) -> v)
             $ xpElemNodes "VContract" (xpContent xpAddress)
           , xpWrap (VDateTime . Script.DateTime . unsafeFromJust . parseDatetime,  -- XXX
             \(VDateTime dt) -> formatDatetime (Script.unDateTime dt))
             $ xpElemNodes "VDateTime" (xpContent xpPrim)
           , xpWrap (VTimeDelta. (\(Right v) -> v) . Script.Parser.parseTimeDelta,  -- XXX
             \(VTimeDelta (TimeDelta d)) -> DT.displayDelta d)
             $ xpElemNodes "VTimeDelta" (xpContent xpPrim)
           , xpWrap (VState . Script.Label, \(VState (Script.Label v)) -> v)
             $ xpElemNodes "VState" (xpContent xpPrim)
           , xpWrap (\_ -> VUndefined, \VUndefined -> ())
             $ xpElemNodes "VUndefined" xpUnit
        ]

xpTxAsset :: PU [UNode Text] TxAsset
xpTxAsset =
  xpAlt tag ps
  where
    tag (CreateAsset _ _ _ _) = 0
    tag (Transfer _ _ _) = 1
    tag (Bind  _ _ _) = 2
    ps = [ xpWrap (\((name, supply, reference), type_) -> CreateAsset (SafeString.fromBytes' name) supply (fmap toEnum reference) type_,
                    \(CreateAsset name supply reference type_) -> (((SafeString.toBytes name), supply, (fmap fromEnum reference)), type_))
            $ xpElem "CreateAsset"
              (xpTriple
                (xpAttr "name" xpPrim)
                (xpAttr "supply" xpPrim)
                (xpOption $ xpAttr "reference" xpPrim))
                xpAssetType
         , xpWrap (\(address, to, balance) -> Transfer address to balance,
                    \(Transfer address to balance) -> (address, to, balance))
            $ xpElemAttrs "Transfer"
              (xpTriple
                (xpAttr "address" xpAddress)
                (xpAttr "to" xpPrim)
                (xpAttr "balance" xpPrim))
         , xpWrap (\(assetAddr, contractAddr, bindProof) -> Bind assetAddr contractAddr bindProof,
                    \(Bind assetAddr contractAddr bindProof) -> (assetAddr, contractAddr, bindProof))
            $ xpElemAttrs "Bind"
              (xpTriple
                (xpAttr "assetAddr" xpAddress)
                (xpAttr "contractAddr" xpAddress)
                (xpAttr "bindProof" xpPrim))
      ]

xpAssetType :: PU [UNode Text] AssetType
xpAssetType =
  xpAlt tag ps
  where
    tag Discrete = 0
    tag (Fractional _) = 1
    tag Binary = 2
    ps = [xpWrap (\_ -> Discrete, \Discrete -> ())
           $ xpElemNodes "Discrete" xpUnit

        , xpWrap (Fractional, \(Fractional prec) -> prec)
          $ xpElemAttrs "Fractional"
            (xpAttr "prec" xpPrim)

        , xpWrap (\_ -> Binary, \Binary -> ())
           $ xpElemNodes "Binary" xpUnit
      ]

xpTxAccount :: PU [UNode Text] TxAccount
xpTxAccount =
  xpAlt tag ps
  where
    tag (CreateAccount _ _ _) = 0
    tag (RevokeAccount _) = 1
    ps = [ xpWrap (\((pubkey, timezone), metadata) -> CreateAccount pubkey timezone metadata,
           \(CreateAccount pubkey timezone metadata) -> ((pubkey, timezone), metadata))
          $ xpElem "CreateAccount"
            (xpPair
              (xpAttr "pubkey" xpPrim)
              (xpAttr "timezone" xpPrim))
            (xpMap "key" "value" xpByteString (xpContent xpByteString))
         , xpWrap (RevokeAccount, \(RevokeAccount address) -> address)
          $ xpElemAttrs "RevokeAccount"
            (xpAttr "address" xpAddress)
      ]
