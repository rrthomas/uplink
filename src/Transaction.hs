{-|

Transaction data structures and serialization.

-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Transaction (
  -- ** Types
  Transaction(..),
  Status(..),
  TxAsset(..),
  TxContract(..),
  TxAccount(..),
  TransactionHeader(..),
  SyncLocalOp(..),

  -- ** Creation
  newTransaction,

  -- ** Hashing
  hashTransaction,

  -- ** Address Derivation
  transactionToAddress,

  -- ** Serialization
  encodeTransaction,
  decodeTransaction,

  -- ** Invalid Transactions
  InvalidTransaction(..),
  hashInvalidTx,

  InvalidTxHeader(..),
  InvalidTxAccount(..),
  InvalidTxAsset(..),
  InvalidTxContract(..),
  TxValidationError(..),
  InvalidTxField(..),

  TxHeader(..),
  TxElem(..),
  txElemName,
  txElemToTransactionHeader,

  -- ** Validation / Verification
  verifyTransaction,
  validateTransaction,
) where

import Protolude hiding (to, put, get)

import Hash (sha256)
import Data.Aeson hiding (decode, encode)
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Serialize as S
import qualified Data.Binary as B
import Control.Monad (fail)
import Crypto.Random.Types (MonadRandom(..))

import SafeString (SafeString)
import SafeInteger (SafeInteger, toSafeInteger')

import Asset (Balance, putAssetType, getAssetType, putRef, getRef, Holder(..))
import Address (Address, putAddress, getAddress, AAccount, AAsset, AContract, emptyAddr)
import Metadata (Metadata(..))
import qualified Key
import qualified Time
import qualified Hash
import qualified Asset
import qualified Address
import qualified Contract
import qualified Encoding
import qualified Ledger
import qualified Storage
import qualified SafeString
import qualified Utils

import qualified Script.Eval
import qualified Script.Typecheck
import qualified Script.Compile as Compile

import Transaction.Generics (TxHeader(..), TxElem(..), txElemName, txElemIndex)

import Database.PostgreSQL.Simple.ToField   (ToField(..), Action(..))
import Database.PostgreSQL.Simple.FromField (FromField(..), ResultError(..), returnError)

import qualified Data.List as DL

-------------------------------------------------------------------------------
-- Transaction
-------------------------------------------------------------------------------

-- | Transaction
data Transaction = Transaction
  { header    :: TransactionHeader
  , signature :: Encoding.Base64PByteString
  , origin    :: Address AAccount
  } deriving (Show, Eq, Generic, NFData, Serialize, Hash.Hashable)

-- | Transaction header
data TransactionHeader
  = TxContract TxContract  -- ^ Contract transaction
  | TxAsset TxAsset        -- ^ Asset transactions
  | TxAccount TxAccount    -- ^ Account transactions
  deriving (Show, Eq, Generic, NFData, Hash.Hashable, TxHeader)

data Status
  = Pending     -- ^ Transaction is waiting to be included in a block or rejected
  | Rejected    -- ^ Transaction has been rejected due to invalidity
  | Accepted    -- ^ Transaction has been included in a block
  | NonExistent -- ^ Transaction has never been seen by the network
  deriving (Show, Eq, Generic, NFData, Hash.Hashable, ToJSON, FromJSON)

-------------------------------------------------------------------------------
-- Transaction Types
-------------------------------------------------------------------------------

data TxContract
  = CreateContract {
      contract :: SafeString.SafeString
  }
  | SyncLocal {
      address :: Address AContract
  , op :: SyncLocalOp
  }                                           -- ^ Sync local storage with contract
  | Call {
      address :: Address AContract
    , method  :: ByteString
    , args    :: [Storage.Value]
  }
  deriving (Show, Eq, Generic, NFData, Hash.Hashable, TxHeader)


-- XXX This is currently unused anywhere in Uplink
data SyncLocalOp
  = InitialCommit (SafeInteger.SafeInteger, SafeInteger.SafeInteger)
  | Sync
  | Finalize (Address AContract) SafeInteger.SafeInteger
  deriving (Show, Eq, Generic, NFData, Hash.Hashable, Serialize)


data TxAsset
  = CreateAsset {
      assetName :: SafeString.SafeString          -- ^ Asset name
    , supply    :: Int64                          -- ^ Asset supply
    , reference :: Maybe Asset.Ref                -- ^ Asset reference
    , assetType :: Asset.AssetType                -- ^ Asset type
    , metadata  :: Metadata                       -- ^ Arbitrary additional metadata
  }

  | Transfer {
      assetAddr :: Address AAsset                 -- ^ Address of asset
    , toAddr    :: Holder                         -- ^ Asset of Receiver
    , balance   :: Balance                        -- ^ Amount to transfer
  }

  | Circulate {
      assetAddr :: Address AAsset                 -- ^ Address of asset
    , amount    :: Balance                        -- ^ Amount to transfer
  }

  | Bind {
      assetAddr    :: Address AAsset              -- ^ Asset address
    , contractAddr :: Address AContract           -- ^ Account address
    , bindProof    :: (SafeInteger, SafeInteger)  -- ^ Bind proof
  }

  | RevokeAsset {
      address :: Address AAsset                   -- ^ Address of asset to revoke
  }
  deriving (Show, Eq, Generic, NFData, Hash.Hashable, TxHeader)

data TxAccount
  = CreateAccount {
      pubKey   :: SafeString.SafeString -- ^ Public key associated with the account
    , timezone :: SafeString.SafeString -- ^ Time zone
    , metadata :: Metadata              -- ^ Arbitrary additional metadata
  }
  | RevokeAccount {
      address   :: Address AAccount     -- ^ Issue a revocation of an account
  }
  deriving (Show, Eq, Generic, NFData, Hash.Hashable, TxHeader)

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

instance B.Binary Transaction where
  put = Utils.putBinaryViaSerialize
  get = Utils.getBinaryViaSerialize

instance Serialize TransactionHeader where
  put txHdr = case txElemIndex txHdr of
    Nothing -> panic $ "Can't find TransactionHeader to serialize" <> show txHdr
    Just level1 -> do
      putWord16be level1
      case txHdr of
        TxContract txc -> put txc

        TxAsset txa -> put txa

        TxAccount txa -> put txa

  get = do
    level1 <- fromInteger . toInteger <$> getWord16be
    let txElem = txElemToTransactionHeader $ tHeader (witness :: TransactionHeader) DL.!! level1

    case txElem of
      TxContract _ -> TxContract <$> get
      TxAsset _ -> TxAsset <$> get
      TxAccount _ -> TxAccount <$> get

getTxContract :: TxContract -> Get TxContract
getTxContract CreateContract{} =
  CreateContract <$> get
getTxContract SyncLocal{} = do
  addr <- getAddress
  op   <- get
  pure $ SyncLocal addr op
getTxContract Call{} = do
  address <- getAddress
  method  <- get
  args    <- get
  pure $ Call address method args

getTxAsset :: TxAsset -> Get TxAsset
getTxAsset CreateAsset{} = do
  name   <- get
  supply <- getInt64be
  ref    <- getWord16be
  mRef <-
    if | ref == 0 -> return Nothing
       | ref == 1 -> Just <$> getRef
       | True     -> fail $ show ref <>
           " is not a valid CreateAsset reference prefix."
  assetType <- getAssetType
  md <- get
  pure $ CreateAsset name supply mRef assetType md
getTxAsset Transfer{} = do
  asset <- getAddress
  to    <- getAddress
  bal   <- getInt64be
  pure $ Transfer asset (Holder (to :: Address AAccount)) bal
getTxAsset Circulate{} = do
  assetAddr <- getAddress
  amount    <- getInt64be
  pure $ Circulate assetAddr amount
getTxAsset Bind{} = do
  assetAddr    <- getAddress
  contractAddr <- getAddress
  bindProof    <- get :: Get (SafeInteger, SafeInteger)
  pure $ Bind assetAddr contractAddr bindProof
getTxAsset (RevokeAsset _) = do
  addr <- getAddress
  pure $ RevokeAsset addr

getTxAccount :: TxAccount -> Get TxAccount
getTxAccount CreateAccount{} = do
  pubKey <- get
  tz <- get
  md <- get
  pure $ CreateAccount pubKey tz md
getTxAccount RevokeAccount{} = do
  addr <- getAddress
  pure $ RevokeAccount addr

instance Serialize TxContract where
  put txc = case txElemIndex txc of
    Nothing -> panic $ "Can't find TxContract to serialize" <> show txc
    Just level2 -> case txc of
      CreateContract contract -> do
        putWord16be level2
        put contract

      SyncLocal addr op -> do
        putWord16be level2
        putAddress addr
        put op

      Call address method args -> do
        putWord16be level2
        putAddress address
        put method
        put args
  get = do
    level2 <- fromInteger . toInteger <$> getWord16be
    getTxContract $ txElemToTxContract $
      tHeader (witness :: TxContract) DL.!! level2


instance Serialize TxAsset where
  put txa = case txElemIndex txa of
    Nothing -> panic $ "Can't find TxAsset to serialize" <> show txa
    Just level2 -> case txa of
      CreateAsset name supply mRef assetType md -> do
        putWord16be level2
        put name
        putInt64be supply

        case mRef of
          -- TODO: What's this for?
          Nothing -> putWord16be 0
          Just ref -> do
            putWord16be 1
            putRef ref

        putAssetType assetType
        put md

      Transfer asset (Holder to) bal -> do
        putWord16be level2
        putAddress asset
        putAddress to
        putInt64be bal

      Circulate asset amount -> do
        putWord16be level2
        putAddress asset
        putInt64be amount

      Bind asset act sig -> do
        putWord16be level2
        putAddress asset
        putAddress act
        put sig

      RevokeAsset addr -> do
        putWord16be level2
        putAddress addr
  get = do
    level2 <- fromInteger . toInteger <$> getWord16be
    getTxAsset $ txElemToTxAsset $
      tHeader (witness :: TxAsset) DL.!! level2

instance Serialize TxAccount where
  put txa = case txElemIndex txa of
    Nothing -> panic $ "Can't find TxAccount to serialize" <> show txa
    Just level2 -> case txa of
      CreateAccount pubKey tz md -> do
        putWord16be level2
        put pubKey
        put tz
        put md

      RevokeAccount addr -> do
        putWord16be level2
        putAddress addr
  get = do
    level2 <- fromInteger . toInteger <$> getWord16be
    getTxAccount $ txElemToTxAccount $
      tHeader (witness :: TxAccount) DL.!! level2

txCall :: Address AContract -> ByteString -> [Storage.Value] -> TxContract
txCall = Call

-------------------------------------------------------------------------------
-- JSON Serialization of Transactions
-------------------------------------------------------------------------------

instance ToJSON Transaction where
  toJSON t =
    object
        [ "header"    .= header t
        , "signature" .= signature t
        , "origin"    .= origin t
        ]

instance FromJSON Transaction where
  parseJSON (Object v) = do
    hd     <- v .: "header"
    sig    <- v .: "signature"
    origin <- v .: "origin"
    pure Transaction
      { header    = hd
      , signature = sig
      , origin    = origin
      }
  parseJSON invalid = typeMismatch "Transaction" invalid

-------------------------------------------------------------------------------

instance ToJSON TransactionHeader where
  toJSON (TxAsset txa) = object
    [ "tag"      .= ("TxAsset" :: Text)
    , "contents" .= txa
    ]
  toJSON (TxContract txc) = object
    [ "tag"      .= ("TxContract" :: Text)
    , "contents" .= txc
    ]
  toJSON (TxAccount txa) = object
    [ "tag"      .= ("TxAccount" :: Text)
    , "contents" .= txa
    ]

instance FromJSON TransactionHeader where
  parseJSON (Object v) = do
    tagV <- v .: "tag" :: Parser [Char]
    if | tagV == "TxContract"  -> do
          txa <- v .: "contents"
          return $ TxContract txa
       | tagV == "TxAccount"   -> do
          txa <- v .: "contents"
          return $ TxAccount txa
       | tagV == "TxAsset"     -> do
          txa <- v .: "contents"
          return $ TxAsset txa
  parseJSON invalid = typeMismatch "TransactionHeader" invalid

-------------------------------------------------------------------------------

-- | FromJSON/ToJSON is not idempotent
instance ToJSON TxContract where
  toJSON (CreateContract con) = object
    [ "tag"      .= ("CreateContract" :: Text)
    , "contents" .= object [ "contract" .= con ]
    ]
  toJSON (SyncLocal contractAddr op) = object
    [ "tag"      .= ("SyncLocal" :: Text)
    , "contents" .= object
        [ "address" .= contractAddr
        , "op" .= op
        ]
    ]

  toJSON (Call addr method args) = object
    [ "tag"      .= ("Call" :: Text)
    , "contents" .= object
      [ "address" .= addr
      , "method"  .= decodeUtf8 method
      , "args"    .= args
      ]
    ]

-- XXX This is currently unused anywhere in Uplink
instance ToJSON SyncLocalOp where
  toJSON = \case
    InitialCommit (r,s) -> object
      [ "tag" .= ("InitialCommit" :: Text)
      , "contents" .= object
          [ "r" .= r, "s" .= s ]
      ]
    Sync -> object [ "tag" .= ("Sync" :: Text) ]
    Finalize addr x -> object
      [ "tag" .= ("Finalize" :: Text)
      , "contents" .= object [ "address" .= addr, "x" .= x ]
      ]

instance ToJSON TxAsset where
  toJSON (CreateAsset name supply ref assetType md) = object
    [ "tag"      .= ("CreateAsset" :: Text)
    , "contents" .= object
        [ "assetName" .= name
        , "supply"    .= supply
        , "reference" .= ref
        , "assetType" .= assetType
        , "metadata"  .= md
        ]
    ]
  toJSON (Transfer asset to bal) = object
    [ "tag"      .= ("Transfer" :: Text)
    , "contents" .= object
        [ "assetAddr" .= asset
        , "toAddr"    .= to
        , "balance"   .= bal
        ]
    ]
  toJSON (Circulate asset amount) = object
    [ "tag"      .= ("Circulate" :: Text)
    , "contents" .= object
        [ "assetAddr" .= asset
        , "amount"    .= amount
        ]
    ]
  toJSON (Bind asset con sig) = object
    [ "tag"      .= ("Bind" :: Text)
    , "contents" .= object
      [ "asset"    .= asset
      , "contract" .= con
      , "proof"    .= sig
      ]
    ]
  toJSON (RevokeAsset addr) = object
    [ "tag"      .= ("RevokeAsset" :: Text)
    , "contents" .= object
      [ "address" .= addr ]
    ]

instance FromJSON TxAsset where
  parseJSON (Object v) = do
    tagV <- v .: "tag" :: Parser [Char]
    if | tagV == "CreateAsset" -> do
          c <- v .: "contents"
          CreateAsset
            <$> c .: "assetName"
            <*> c .: "supply"
            <*> c .:? "reference"
            <*> c .: "assetType"
            <*> c .: "metadata"

       | tagV == "Bind" -> do
          c <- v .: "contents"
          Bind <$> c .: "asset" <*> c .: "contract" <*> c .: "proof"

       | tagV == "Transfer" -> do
          c <- v .: "contents"
          Transfer <$> c .: "assetAddr"
                   <*> c .: "toAddr"
                   <*> c .: "balance"

       | tagV == "Circulate" -> do
           c <- v .: "contents"
           Circulate <$> c .: "assetAddr"
                     <*> c .: "amount"

       | tagV == "RevokeAsset" -> do
           c <- v .: "contents"
           RevokeAsset <$> c .: "address"

       | otherwise -> fail $ "Uknown tag in TxAsset: " <> tagV

  parseJSON invalid = typeMismatch "TxAsset" invalid

instance FromJSON TxContract where
  parseJSON = \case
    Object o -> do
      tag <- o .: "tag" :: Parser [Char]
      case tag of
        "CreateContract" -> do
          c <- o .: "contents"
          script <- c .: "contract"
          -- Parse and type check script XXX Should we do this here?
          case Compile.compile (toS (SafeString.toBytes script)) of
            Left err -> fail (toS err)
            Right _  -> pure (CreateContract script)

        "Call" -> do
          c <- o .: "contents"
          Call <$> c .: "address"
               <*> liftA encodeUtf8 (c .: "method")
               <*> c .: "args"

        "SyncLocal" -> do
          c <- o .: "contents"
          contractAddr <- c .: "address"
          syncLocalOp  <- c .: "op"
          pure $ SyncLocal contractAddr syncLocalOp

        invalid -> fail $ "Invalid TxContract 'tag' field: " <> invalid

    invalid -> typeMismatch "TxContract" invalid

instance FromJSON SyncLocalOp where
  parseJSON = \case
    Object o -> do
      tag <-  o .: "tag" :: Parser Text
      case tag of
        "InitialCommit" -> do
          c <- o .: "contents"
          r <- c .: "r"
          s <- c .: "s"
          pure $ InitialCommit (r,s)
        "Sync" -> pure Sync
        "Finalize" -> do
          c    <- o .: "contents"
          addr <- c .: "address"
          x    <- c .: "x"
          pure $ Finalize addr x
        invalid -> fail $ "Invalid SyncLocalOp 'tag' field: " <> toS invalid
    invalid -> typeMismatch "SyncLocalOp" invalid

instance ToJSON TxAccount where
  toJSON (CreateAccount pk tz md) = object
    [ "tag"      .= ("CreateAccount" :: Text)
    , "contents" .= object
      [ "pubKey"     .= pk
      , "timezone"   .= tz
      , "metadata"   .= md
      ]
    ]

  toJSON (RevokeAccount addr) = object
    [ "tag"      .= ("RevokeAccount" :: Text)
    , "contents" .= object [ "address" .= addr ]
    ]

instance FromJSON TxAccount where
  parseJSON (Object v) = do
    tagV <- v .: "tag" :: Parser [Char]
    contents <- v .: "contents"
    case tagV of
      "CreateAccount" -> do
        pk <- contents .: "pubKey"
        tz <- contents .: "timezone"
        md <- contents .: "metadata"
        pure $ CreateAccount pk tz md
      "RevokeAccount" -> RevokeAccount <$> contents .: "address"
      _ -> fail $ "Unknown tag in TxAccount: " <> tagV

  parseJSON invalid = typeMismatch "TxAccount" invalid

-- Necessary instance because Data.Serialize.encode/decode does not play well
-- with postgresql-simple's ByteString-to-bytea serializer
instance ToField TransactionHeader where
  toField = EscapeByteA . S.encode

instance FromField TransactionHeader where
  fromField f mdata = do
    bs <- fromField f mdata
    case S.decode <$> bs of
      Nothing            -> returnError UnexpectedNull f ""
      Just (Left err)    -> returnError ConversionFailed f err
      Just (Right txHdr) -> return txHdr

-- | Serialize transaction
encodeTransaction :: Transaction -> ByteString
encodeTransaction = encode

-- | Deserialize transaction
decodeTransaction :: ByteString -> Either [Char] Transaction
decodeTransaction = decode

-- | Hash transaction
-- Transactions currently use base 16 encoding.
hashTransaction :: Transaction -> Hash.Hash Encoding.Base16ByteString
hashTransaction = Hash.toHash

-- | Computes a Ledger value address using a transaction hash:
-- base58Encode ( sha3_256 ( base16Encode ( sha3_256 ( binary ( TX ) ) ) ) )
transactionToAddress :: Transaction -> Address a
transactionToAddress =
  Address.Address . Hash.sha256 . Hash.getRawHash . hashTransaction

-------------------------------------------------------------------------------
-- Creation
-------------------------------------------------------------------------------

-- | Create a new transaction.
newTransaction
  :: MonadRandom m
  => Address AAccount   -- ^ Origin Account Address
  -> Key.PrivateKey     -- ^ Private Key
  -> TransactionHeader  -- ^ Transaction payload
  -> m Transaction
newTransaction origin privKey header = do
  sig    <- Key.sign privKey $ S.encode header
  pure Transaction {
    header    = header
  , origin    = origin
  , signature = Key.encodeSig sig
  }

-- | Sign a transaction with a private key
signTransaction
  :: MonadRandom m
  => Key.PrivateKey
  -> Transaction
  -> m Key.Signature
signTransaction key = Key.sign key . S.encode . header

-------------------------------------------------------------------------------
-- Validation / Verification
-------------------------------------------------------------------------------

{-

Validates a transaction without respect to a Ledger State

* Timestamp is valid
* Signature is valid

-}

data InvalidTransaction = InvalidTransaction
  { transaction :: Transaction
  , reason      :: TxValidationError
  } deriving (Show, Eq, Generic, S.Serialize)

instance ToJSON InvalidTransaction where
  toJSON (InvalidTransaction tx inv) = object
    [ "transaction" .= tx
    , "reason"      .= (show inv :: Text)
    ]

hashInvalidTx :: InvalidTransaction -> Hash.Hash Encoding.Base16ByteString
hashInvalidTx (InvalidTransaction tx err) = hashTransaction tx

data InvalidTxField
  = InvalidTxTimestamp Time.Timestamp
  | InvalidTxSignature Key.InvalidSignature
  deriving (Show, Eq, Generic, S.Serialize)

data InvalidTxAccount
  = InvalidPubKeyByteString ByteString
  | RevokeValidatorError (Address AAccount)
  | AccountError Ledger.AccountError
  deriving (Show, Eq, Generic, S.Serialize)

data InvalidTxAsset
  = AssetError Ledger.AssetError
  deriving (Show, Eq, Generic, S.Serialize)

data InvalidTxContract
  = InvalidContract [Char]
  | ContractError Ledger.ContractError
  | InvalidMethodName Contract.InvalidMethodName
  | InvalidCallArgType Script.Typecheck.TypeErrInfo
  | EvalFail Script.Eval.EvalFail
  deriving (Show, Eq, Generic, S.Serialize)

data InvalidTxHeader
  = InvalidTxAccount InvalidTxAccount
  | InvalidTxAsset InvalidTxAsset
  | InvalidTxContract InvalidTxContract
  deriving (Show, Eq, Generic, S.Serialize)

data TxValidationError
  = NoSuchOriginAccount (Address AAccount)  -- ^ Origin account does not exist on the ledger
  | DuplicateTransaction                  -- ^ Transaction is not unique
  | InvalidTxField InvalidTxField         -- ^ One of the transaction fields is invalid
  | InvalidTxHeader InvalidTxHeader       -- ^ The transaction header is invalid
  | InvalidPubKey                         -- ^ The public key cannot be decoded
  deriving (Show, Eq, Generic, S.Serialize)

-- | Verify a transaction with a public key
verifyTransaction
  :: Key.PubKey
  -> Transaction
  -> Either Key.InvalidSignature ()
verifyTransaction key t = do
  let encodedHeader = S.encode (header t)
  case Key.decodeSig (signature t) of
    Left err -> Left err
    Right sig
      | Key.verify key sig encodedHeader -> Right ()
      | otherwise -> Left $ Key.InvalidSignature sig encodedHeader

-- | Validate a transaction without looking up the origin account
-- by using cryptography magic (public key recovery from signature).
validateTransaction :: Transaction -> Either InvalidTransaction ()
validateTransaction tx@Transaction{..} =
    -- Validate transaction signature
    first (mkInvalidTx . InvalidTxSignature) $
      let headerBS = S.encode header in
      case Address.recoverAddress signature headerBS of
        Left err -> Left err
        Right (addr1, addr2)
          | origin == addr1 || origin == addr2 -> Right ()
          | otherwise -> case Key.decodeSig signature of
              Left err -> Left err
              Right sig -> Left $ Key.InvalidSignature sig headerBS
  where
    mkInvalidTx = InvalidTransaction tx . InvalidTxField

-------------------------------------------------------------------------------
-- Serialization (InvalidTransaction)
-------------------------------------------------------------------------------

-- Necessary instances because Data.Serialize.encode/decode does not play well
-- with postgresql-simple's ByteString-to-bytea serializer
instance ToField TxValidationError where
  toField = EscapeByteA . S.encode

instance FromField TxValidationError where
  fromField f mdata = do
    bs <- fromField f mdata
    case S.decode <$> bs of
      Nothing            -> returnError UnexpectedNull f ""
      Just (Left err)    -> returnError ConversionFailed f err
      Just (Right txErr) -> return txErr

-------------------------------------------------------------------------------
-- Convert generic transaction elems to transaction types
-------------------------------------------------------------------------------

txElemToTxAccount :: TxElem -> TxAccount
txElemToTxAccount e = if
  | e == txElemName createAccountEmpty -> createAccountEmpty
  | e == txElemName revokeAccountEmpty -> revokeAccountEmpty
  | otherwise -> panic "Invalid TxAccount"

txElemToTxAsset :: TxElem -> TxAsset
txElemToTxAsset e = if
  | e == txElemName createAssetEmpty -> createAssetEmpty
  | e == txElemName revokeAssetEmpty -> revokeAssetEmpty
  | e == txElemName transferEmpty -> transferEmpty
  | e == txElemName circulateEmpty -> circulateEmpty
  | e == txElemName bindEmpty -> bindEmpty
  | otherwise -> panic "Invalid TxAsset"

txElemToTxContract :: TxElem -> TxContract
txElemToTxContract e = if
  | e == txElemName createContractEmpty -> createContractEmpty
  | e == txElemName syncLocalEmpty -> syncLocalEmpty
  | e == txElemName callEmpty -> callEmpty
  | otherwise -> panic "Invalid TxContract"

txElemToTransactionHeader :: TxElem -> TransactionHeader
txElemToTransactionHeader e =  if
  | e == txElemName txContractEmpty -> txContractEmpty
  | e == txElemName txAccountEmpty -> txAccountEmpty
  | e == txElemName txAssetEmpty -> txAssetEmpty
  | otherwise -> panic "Invalid TxHeaderType flag"

-------------------------------------------------------------------------------
-- Empty transaction elements
-------------------------------------------------------------------------------

-- TxAccount
createAccountEmpty :: TxAccount
createAccountEmpty = CreateAccount mempty mempty mempty

revokeAccountEmpty :: TxAccount
revokeAccountEmpty = RevokeAccount emptyAddr

-- TxContract
createContractEmpty :: TxContract
createContractEmpty = CreateContract mempty

syncLocalEmpty :: TxContract
syncLocalEmpty = SyncLocal emptyAddr Sync

callEmpty :: TxContract
callEmpty = Call emptyAddr mempty mempty

-- TxAsset
createAssetEmpty :: TxAsset
createAssetEmpty = CreateAsset mempty 0 Nothing Asset.Discrete mempty

revokeAssetEmpty :: TxAsset
revokeAssetEmpty = RevokeAsset emptyAddr

transferEmpty :: TxAsset
transferEmpty = Transfer emptyAddr (Holder (emptyAddr :: Address AAccount)) 0

circulateEmpty :: TxAsset
circulateEmpty = Circulate emptyAddr 0

bindEmpty :: TxAsset
bindEmpty = Bind emptyAddr emptyAddr (toSafeInteger' 0, toSafeInteger' 0)

-- transactionHeader
txContractEmpty :: TransactionHeader
txContractEmpty = TxContract createContractEmpty

txAssetEmpty :: TransactionHeader
txAssetEmpty = TxAsset createAssetEmpty

txAccountEmpty :: TransactionHeader
txAccountEmpty = TxAccount createAccountEmpty

instance B.Binary InvalidTransaction where
  put = Utils.putBinaryViaSerialize
  get = Utils.getBinaryViaSerialize
