{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module XML (
  ToXML(..),
) where

import Protolude

import Text.XML.Expat.Pickle
import Text.XML.Expat.Format

import qualified Data.Map as Map

import Block
import Asset
import Script
import Address
import Metadata (Metadata(..))
import SafeString
import SafeInteger
import Transaction
import Script.Parser
import Datetime.Types
import Consensus.Authority.Params
import qualified Key
import qualified Encoding
import qualified Hash
import qualified Fixed
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

renderXML :: Text -> PU [Node Text Text] a -> [a] -> LByteString
renderXML label f v = format (indent 4 (pickleTree body v))
  where
    body = xpRoot (xpElemNodes label $ xpList f)

-------------------------------------------------------------------------------
-- Serializers
-------------------------------------------------------------------------------

xpAddress :: PU Text (Address a)
xpAddress =
  xpWrap (fromRaw, rawAddr) xpByteString

xpHolder :: PU Text Holder
xpHolder =
  xpWrap (toHolder, holderToAccount) xpAddress
  where
    -- The address tag is arbitrary, to make the type checker happy
    toHolder addr = Holder (addr :: Address AAccount)

xpByteString :: PU Text ByteString
xpByteString =
  xpWrap (encodeUtf8, decodeUtf8) xpText0

xpBase64PByteString :: PU Text Encoding.Base64PByteString
xpBase64PByteString =
  xpWrap (base64P . encodeUtf8, decodeUtf8 . Encoding.unbase64P) xpText0
  where
    base64P bs = case Encoding.parseEncodedBS bs of
      Left err -> panic $ "Base64PByteString encoding error: " <> toS bs
      Right b -> b

xpHash :: Encoding.ByteStringEncoding a => PU Text (Hash.Hash a)
xpHash =
  xpWrap (Hash.toHash . encodeUtf8, decodeUtf8 . Hash.getRawHash) xpText0

xpMetadata :: PU [Node Text Text] Metadata
xpMetadata =
  xpWrap (Metadata, unMetadata) $
    xpMap "key" "value" xpText (xpContent xpText)

xpBlock :: PU [Node Text Text] Block
xpBlock =
   xpWrap (\(index, (signatures, header, transactions)) -> Block index header (Set.fromList $signatures) transactions,
           \(Block index header signatures transactions) -> (index, (Set.toList signatures, header, transactions))) $
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
      (xpAttr "prevHash" xpHash)
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
    sigToText = toS . Encoding.unbase64P . Key.encodeSig

    textToSig :: Text -> Key.Signature
    textToSig = Key.decodeSig' . base64P . toS

    base64P :: ByteString -> Encoding.Base64PByteString
    base64P bs = case Encoding.parseEncodedBS bs of
      Left err -> panic $ "Base64PByteString encoding error: " <> toS bs
      Right b -> b

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
   xpWrap (\((signature, origin), header) -> Transaction header signature origin,
           \(Transaction header signature origin) -> ((signature, origin), header)) $
   xpElem "transaction"
       (xpPair
           (xpAttr "signature" xpBase64PByteString)
           (xpAttr "origin" xpPrim)
       ) xpTransactionHeader

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
    tag (CreateContract _) = 0
    tag (SyncLocal _ _) = 1
    tag (Call  _ _ _) = 2
    ps = [ xpWrap (\contract -> CreateContract (SafeString.fromBytes' contract),
                    \(CreateContract contract) -> (SafeString.toBytes contract))
            $ xpElemNodes "CreateContract" (xpContent xpPrim)
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
      tag (VFloat _)     = 1
      tag (VFixed _)     = 2
      tag (VBool _)      = 3
      tag VVoid          = 4
      tag (VSig _)       = 5
      tag (VMsg _)       = 6
      tag (VAccount _)   = 7
      tag (VAsset _)     = 8
      tag (VContract _)  = 9
      tag (VDateTime _)  = 10
      tag (VTimeDelta _) = 11
      tag (VState _)     = 12
      tag VUndefined     = 13
      tag (VEnum _)      = 14
      tag (VMap _)       = 15
      tag (VSet _)       = 16
      ps = [ xpWrap (VInt, \(VInt v) -> v)
             $ xpElemNodes "VInt" (xpContent xpPrim)
           , xpWrap (VFloat, \(VFloat v) -> v)
             $ xpElemNodes "VFloat" (xpContent xpPrim)
           , xpWrapEither (first show . fmap VFixed . Script.Parser.parseFixedN,
             \(VFixed v) -> Pretty.prettyPrint v) $ xpElemNodes "VFixed" (xpContent xpPrim)
           , xpWrap (VBool, \(VBool v) -> v)
             $ xpElemNodes "VBool" (xpContent xpPrim)
           , xpWrap (\_ -> VVoid, \VVoid -> ())
             $ xpElemNodes "VVoid" xpUnit
           , xpWrap (VSig, \(VSig v) -> v)
             $ xpElemNodes "VSig" (xpContent xpPrim)
           , xpWrap (VMsg, \(VMsg v) -> v)
             $ xpElemNodes "VMsg" (xpContent xpPrim)
           , xpWrap (VAccount, \(VAccount v) -> v)
             $ xpElemNodes "VAccount" (xpContent xpAddress)
           , xpWrap (VAsset, \(VAsset v) -> v)
             $ xpElemNodes "VAsset" (xpContent xpAddress)
           , xpWrap (VContract, \(VContract v) -> v)
             $ xpElemNodes "VContract" (xpContent xpAddress)
           , xpWrapMaybe (fmap (VDateTime . Script.DateTime) . parseDatetime . toS,
             \(VDateTime dt) -> toS $ formatDatetime (Script.unDateTime dt))
             $ xpElemNodes "VDateTime" (xpContent xpText)
           , xpWrapEither (first show . fmap VTimeDelta . Script.Parser.parseTimeDelta,
             \(VTimeDelta (TimeDelta d)) -> DT.displayDelta d)
             $ xpElemNodes "VTimeDelta" (xpContent xpPrim)
           , xpWrap (VState . Script.Label, \(VState (Script.Label v)) -> v)
             $ xpElemNodes "VState" (xpContent xpPrim)
           , xpWrap (\_ -> VUndefined, \VUndefined -> ())
             $ xpElemNodes "VUndefined" xpUnit
           , xpWrap (VEnum . EnumConstr, \(VEnum c) -> unEnumConstr c)
             $ xpElemNodes "VEnum" (xpContent xpPrim)
           , xpWrap (VMap . Map.fromList, \(VMap vmap) -> Map.toList vmap)
             $ xpElemNodes "VMap" (xpList (xpPair xpValue xpValue))
        ]

xpTxAsset :: PU [UNode Text] TxAsset
xpTxAsset =
  xpAlt tag ps
  where
    tag (CreateAsset _ _ _ _ _) = 0
    tag (Transfer _ _ _)        = 1
    tag (Circulate _ _)         = 2
    tag (Bind  _ _ _)           = 3
    tag (RevokeAsset _)         = 4

    getPrec :: AssetType -> Maybe Fixed.PrecN
    getPrec type_  = case type_ of
      Fractional p -> Just p
      _            -> Nothing

    ps = [ xpWrap (\((name, supply, reference, type_, prec), metadata) -> CreateAsset (SafeString.fromBytes' name) supply (fmap toEnum reference) type_ metadata,
                  \(CreateAsset name supply reference type_ metadata) -> (((SafeString.toBytes name), supply, (fmap fromEnum reference), type_, getPrec type_), metadata))
          $ xpElem "CreateAsset"
              (xp5Tuple
              (xpAttr "assetName" xpPrim)
              (xpAttr "supply" xpPrim)
              (xpOption $ xpAttr "reference" xpPrim)
              (xpAttr "assetType" xpPrim)
              (xpOption $ xpAttr "assetPrec" xpPrim)
              )
              (xpMetadata)
       , xpWrap (\(address, to, balance) -> Transfer address to balance,
                  \(Transfer address to balance) -> (address, to, balance))
          $ xpElemAttrs "Transfer"
            (xpTriple
              (xpAttr "address" xpAddress)
              (xpAttr "to" xpHolder)
              (xpAttr "balance" xpPrim))
       , xpWrap (\(assetAddr, amount) -> Circulate assetAddr amount,
                  \(Circulate assetAddr amount) -> (assetAddr, amount))
          $ xpElemAttrs "Circulate"
            (xpPair
              (xpAttr "assetAddr" xpAddress)
              (xpAttr "amount" xpPrim))
       , xpWrap (\(assetAddr, contractAddr, bindProof) -> Bind assetAddr contractAddr bindProof,
                  \(Bind assetAddr contractAddr bindProof) -> (assetAddr, contractAddr, bindProof))
          $ xpElemAttrs "Bind"
            (xpTriple
                (xpAttr "assetAddr" xpAddress)
                (xpAttr "contractAddr" xpAddress)
                (xpAttr "bindProof" xpPrim))
         , xpWrap (RevokeAsset, \(RevokeAsset addr) -> addr)
          $ xpElemAttrs "RevokeAsset"
            (xpAttr "address" xpAddress)
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
            xpMetadata
         , xpWrap (RevokeAccount, \(RevokeAccount address) -> address)
          $ xpElemAttrs "RevokeAccount"
            (xpAttr "address" xpAddress)
      ]
