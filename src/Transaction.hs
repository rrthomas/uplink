{-|

Transaction data structures and serialization.

-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DuplicateRecordFields #-}

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
  base16HashTransaction,

  -- ** Serialization
  encodeTransaction,
  decodeTransaction,

  -- ** Invalid Transactions
  InvalidTransaction(..),
  hashInvalidTx,
  base16HashInvalidTx,

  InvalidTxHeader(..),
  InvalidTxAccount(..),
  InvalidTxAsset(..),
  InvalidTxContract(..),
  TxValidationError(..),
  InvalidTxField(..),

  TxValidationCtx(..),

  -- ** Validation / Verification
  verifyTransaction,
  validateTransaction,
  validateTransactionNoTs,

) where

import Protolude hiding (from, to, put, get, putByteString)
import Unsafe (unsafeFromJust)

import Hash (sha256)
import Data.Aeson hiding (decode, encode)
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Serialize as S
import Data.Serialize.Put
import qualified Data.Binary as Binary
import Data.Maybe (fromJust)
import Control.Monad (fail)
import Utils (toInt, toWord16)

import SafeString (SafeString)
import SafeInteger (SafeInteger, fromSafeInteger)

import Asset (Asset, Balance, putAssetType, getAssetType, putRef, getRef)
import Address (Address, putAddress, getAddress)
import Account (Metadata(..))
import Datetime.Types
import qualified Key
import qualified Time
import qualified Hash
import qualified Asset
import qualified Address
import qualified Encoding
import qualified Ledger
import qualified Storage
import qualified SafeString
import qualified SafeInteger
import qualified Script

import Script (Value(..))
import qualified Script.Init
import qualified Script.Eval
import qualified Script.Typecheck

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as Map

import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree

import Database.PostgreSQL.Simple.ToField   (ToField(..), Action(..))
import Database.PostgreSQL.Simple.FromField (FromField(..), ResultError(..), returnError)

-------------------------------------------------------------------------------
-- Transaction
-------------------------------------------------------------------------------

-- | Transaction
data Transaction = Transaction
  { header    :: TransactionHeader
  , signature :: ByteString
  , origin    :: Address
  , timestamp :: Time.Timestamp
  } deriving (Show, Eq, Generic, NFData, Serialize, Hash.Hashable)

-- | Transaction header
data TransactionHeader
  = TxContract TxContract  -- ^ Contract transaction
  | TxAsset TxAsset        -- ^ Asset transactions
  | TxAccount TxAccount    -- ^ Account transactions
  deriving (Show, Eq, Generic, NFData, Hash.Hashable)

data Status
  = Pending
  | Rejected
  | Accepted
  deriving (Show, Eq, Generic, NFData, Hash.Hashable, ToJSON, FromJSON)

-------------------------------------------------------------------------------
-- Transaction Types
-------------------------------------------------------------------------------

data TxContract
  = CreateContract {
      address  :: Address
    , contract :: SafeString.SafeString
  }
  | SyncLocal {
      address :: Address
    , op :: SyncLocalOp
  }                                           -- ^ Sync local storage with contract
  | Call {
      address :: Address
    , method  :: ByteString
    , args    :: [Storage.Value]
  }
  deriving (Show, Eq, Generic, NFData, Hash.Hashable)

data SyncLocalOp
  = InitialCommit (SafeInteger.SafeInteger, SafeInteger.SafeInteger)
  | Sync
  | Finalize Address SafeInteger.SafeInteger
  deriving (Show, Eq, Generic, NFData, Hash.Hashable)

data TxAsset
  = CreateAsset {
      assetAddr :: Address                    -- ^ Address of asset
    , assetName :: SafeString.SafeString      -- ^ Asset name
    , supply    :: Int64                      -- ^ Asset supply
    , reference :: Maybe Asset.Ref            -- ^ Asset reference
    , assetType :: Asset.AssetType            -- ^ Asset type
  }

  | Transfer {
      assetAddr :: Address                    -- ^ Address of asset
    , toAddr    :: Address                    -- ^ Asset of Receiver
    , balance   :: Balance                    -- ^ Amount to transfer
  }

  | Circulate {
      assetAddr :: Address                    -- ^ Address of asset
    , amount    :: Balance                    -- ^ Amount to transfer
  }

  | Bind {
      assetAddr    :: Address                    -- ^ Asset address
    , contractAddr :: Address                    -- ^ Account address
    , bindProof    :: (SafeInteger, SafeInteger) -- ^ Bind proof
  }

  | RevokeAsset {
      address :: Address                      -- ^ Address of asset to revoke
  }
  deriving (Show, Eq, Generic, NFData, Hash.Hashable)

data TxAccount
  = CreateAccount {
      pubKey   :: ByteString                  -- ^ Public key associated with the account
    , timezone :: ByteString                  -- ^ Time zone
    , metadata :: Metadata                    -- ^ Arbitrary additional metadata
  }
  | RevokeAccount {
      address   :: Address                    -- ^ Issue a revocation of an account
  }
  deriving (Show, Eq, Generic, NFData, Hash.Hashable)

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

{- XXX

Currently every time a new transaction is added:

  1) The programmer must manually change the Serialization instance for Transaction
     header, juggling the binary encoding flag denoting which constructor/transaction
     header.

  2) get<TxHeaderType> functions must be changed, incorporating the binary encoding
     flag shift that is necessary in this encoding model.

  3) put<TxHeaderType> functions must be changed, for the same reason listed in the
     description of changes for get<TxHeaderType>

XXX
Suggestion: Serialization of Transaction headers should be rewritten to take advantage
of types; We can use Data.Typeable `typeRep` to get the number of constructors and pass
that to helper functions that assign binary encoding flags to each constructor (for Put).
For get, we could use the typerep to see what constructor the flag corresponds to.
XXX

-}

instance Binary.Binary Transaction where
  put tx = Binary.put $ encode tx
  get = do
    bs <- Binary.get
    case decode bs of
      (Right tx) -> return tx
      (Left err) -> fail err

instance Serialize TransactionHeader where
  put txHdr = case txHdr of
    TxContract txc -> put txc

    TxAsset txa -> put txa

    TxAccount txa -> put txa

  get = do
    code <- getWord16be
    let code' = fromInteger $ toInteger code
    if | code `elem` [1000, 1001, 1002] ->
          getTxContract code'

       | code `elem` [1003, 1004, 1005, 1006, 1007] ->
          getTxAsset code'

       | code `elem` [1008, 1009] ->
          getTxAccount code'

       | otherwise -> fail $
           "Invalid TxHeaderType flag: " <> show code


getTxContract :: Int -> Get TransactionHeader
getTxContract 1000 = do
  addr     <- getAddress
  contract <- get
  pure $ TxContract $
    CreateContract addr contract
getTxContract 1001 = do
  addr <- getAddress
  op   <- get
  pure $ TxContract $
    SyncLocal addr op
getTxContract 1002 = do
  address <- getAddress
  method  <- get
  args    <- get
  pure $ TxContract $
    Call address method args
getTxContract n = fail $ "getTxContract " <> show n

getTxAsset :: Int -> Get TransactionHeader
getTxAsset 1003 = do
  addr   <- getAddress
  name   <- SafeString.getSafeString
  supply <- getInt64be
  ref    <- getWord16be
  mRef <- do
    if | ref == 0 -> return Nothing
       | ref == 1 -> Just <$> getRef
       | True     -> fail $ show ref <>
           " is not a valid CreateAsset reference prefix."
  assetType <- getAssetType
  pure $ TxAsset $
    CreateAsset addr name supply mRef assetType
getTxAsset 1004 = do
  asset <- getAddress
  to    <- getAddress
  bal   <- getInt64be
  pure $ TxAsset $
    Transfer asset to bal
getTxAsset 1005 = do
  assetAddr <- getAddress
  amount    <- getInt64be
  pure $ TxAsset $
    Circulate assetAddr amount
getTxAsset 1006 = do
  assetAddr    <- getAddress
  contractAddr <- getAddress
  bindProof    <- get :: Get (SafeInteger, SafeInteger)
  pure $ TxAsset $
    Bind assetAddr contractAddr bindProof
getTxAsset 1007 = do
  addr <- getAddress
  pure $ TxAsset $
    RevokeAsset addr
getTxAsset n = fail $ "getTxAsset " <> show n

getTxAccount :: Int -> Get TransactionHeader
getTxAccount 1008 = do
  pubKeyLen <- getWord16be
  pubKey <- getBytes $ (toInt pubKeyLen :: Int)
  tzLen <- getWord16be
  tz <- getBytes $ toInt tzLen
  md <- get
  pure $ TxAccount $ CreateAccount pubKey tz md
getTxAccount 1009 = do
  addr <- getAddress
  pure $ TxAccount $
    RevokeAccount addr
getTxAccount n = fail $ "getTxAccount " <> show n

instance Serialize TxContract where
  put txc = case txc of
    CreateContract addr contract -> do
      putWord16be 1000
      putAddress addr
      put contract

    SyncLocal addr op -> do
      putWord16be 1001
      putAddress addr
      put op

    Call address method args -> do
      putWord16be 1002
      putAddress address
      put method
      put args

instance Serialize TxAsset where
  put txa = case txa of
    CreateAsset addr name supply mRef assetType -> do
      putWord16be 1003
      putAddress addr
      SafeString.putSafeString name
      putInt64be supply

      case mRef of
        Nothing -> putWord16be 0
        Just ref -> do
          putWord16be 1
          putRef ref

      putAssetType assetType

    Transfer asset to bal -> do
      putWord16be 1004
      putAddress asset
      putAddress to
      putInt64be bal

    Circulate asset amount -> do
      putWord16be 1005
      putAddress asset
      putInt64be amount

    Bind asset act sig -> do
      putWord16be 1006
      putAddress asset
      putAddress act
      put sig

    RevokeAsset addr -> do
      putWord16be 1007
      putAddress addr

instance Serialize TxAccount where
  put txa = case txa of
    CreateAccount pubKey tz md -> do
      putWord16be 1008
      putWord16be $ toWord16 $ BS.length pubKey
      putByteString pubKey
      putWord16be $ toWord16 $ BS.length tz
      putByteString tz
      put md

    RevokeAccount addr -> do
      putWord16be 1009
      putAddress addr

-- XXX incomplete, only deserialized InitialCommit
instance Serialize SyncLocalOp where
  put op = case op of
        (InitialCommit sig) -> put sig
        _ -> undefined
  get = InitialCommit <$> get

txCall :: Address -> ByteString -> [Storage.Value] -> TxContract
txCall = Call

-------------------------------------------------------------------------------
-- JSON Serialization of Transactions
-------------------------------------------------------------------------------

instance ToJSON Transaction where
  toJSON t =
    object $
        [ "header"    .= header t
        , "signature" .= decodeUtf8 (signature t)
        , "timestamp" .= timestamp t
        , "origin"    .= origin t
        ]

instance FromJSON Transaction where
  parseJSON (Object v) = do
    hd     <- v .: "header"
    sig    <- v .: "signature"
    origin <- v .: "origin"
    ts     <- v .: "timestamp"
    pure $ Transaction
      { header    = hd
      , signature = encodeUtf8 sig
      , origin    = origin
      , timestamp = ts
      }

  parseJSON invalid = typeMismatch "Transaction" invalid

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

-- | FromJSON/ToJSON is not idempotent
instance ToJSON TxContract where
  toJSON (CreateContract addr con) = object
    [ "tag"      .= ("CreateContract" :: Text)
    , "contents" .= object
        [ "address"  .= addr
        , "contract" .= con
        ]
    ]
  toJSON (SyncLocal contractAddr op) = object
    [ "tag"      .= ("SyncLocal" :: Text)
    , "address"  .= contractAddr
    , "contents" .= contents
    ]
    where
      contents = case op of
                    (InitialCommit (x,y)) -> object
                      [
                        "tag" .= ("InitialCommit" ::Text)
                      ,  "x" .= x
                      ,  "y" .= y
                      ]
                    _ -> undefined  --- XXX handle other transaction types

  toJSON (Call addr method args) = object
    [ "tag"      .= ("Call" :: Text)
    , "contents" .= object
      [ "address" .= addr
      , "method"  .= decodeUtf8 method
      , "args"    .= args
      ]
    ]

instance ToJSON TxAsset where
  toJSON (CreateAsset addr name supply ref assetType) = object
    [ "tag"      .= ("CreateAsset" :: Text)
    , "contents" .= object
        [ "assetAddr" .= addr
        , "assetName" .= (toS $ SafeString.toBytes name :: [Char])
        , "supply"    .= supply
        , "reference" .= ref
        , "assetType" .= assetType
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
      , "proof"    .= (toS (Key.hexRS $ bimap fromSafeInteger fromSafeInteger sig) :: Text)
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
            <$> c .: "assetAddr"
            <*> c .: "assetName"
            <*> c .: "supply"
            <*> c .:? "reference"
            <*> c .: "assetType"

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

-- | FromJSON/ToJSON is not idempotent
instance FromJSON TxContract where
  parseJSON (Object v) = do
    tagV <- v .: "tag" :: Parser [Char]
    if | tagV == "CreateContract" -> do
          c <- v .: "contents"
          script <- c .: "script"
          addr   <- c .: "address"
          ts     <- c .: "timestamp"
          owner  <- c .: "owner"
          case Script.Init.createContract addr owner ts (toS (SafeString.toBytes script)) of
            Left err -> fail (toS err)
            Right contract -> pure (CreateContract addr script)

       | tagV == "Call" -> do
          c <- v .: "contents"
          Call <$> c .: "address"
               <*> liftA encodeUtf8 (c .: "method")
               <*> c .: "args"

       | otherwise -> fail $ "Unknown tag in TxContract: " <> tagV

  parseJSON x = fail $ "JSON not a TxContract but: \n" <> (show x)

instance ToJSON TxAccount where
  toJSON (CreateAccount pk tz md) = object
    [ "tag"      .= ("CreateAccount" :: Text)
    , "contents" .= object
      [ "pubKey"     .= decodeUtf8 pk
      , "timezone"   .= decodeUtf8 tz
      , "metadata"   .= md
      ]
    ]

  toJSON (RevokeAccount addr) = object
    [ "tag"      .= ("RevokeAccount" :: Text)
    , "contents" .= object
      [
        "address" .= addr
      ]
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
        let pk' = encodeUtf8 pk
            tz' = encodeUtf8 tz
        pure $ CreateAccount pk' tz' md
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
hashTransaction :: Transaction -> Hash.Hash ByteString
hashTransaction = Hash.toHash

base16HashTransaction :: Transaction -> ByteString
base16HashTransaction = Encoding.base16 . Hash.getHash . hashTransaction

-------------------------------------------------------------------------------
-- Creation
-------------------------------------------------------------------------------

-- | Create a new transaction.
newTransaction
  :: Address            -- ^ Origin Account Address
  -> Key.PrivateKey     -- ^ Private Key
  -> TransactionHeader  -- ^ Transaction payload
  -> IO Transaction
newTransaction origin privKey header = do
  ts     <- Time.now
  sig    <- Key.sign privKey $ S.encode header
  pure $ Transaction {
    header    = header
  , origin    = origin
  , signature = Key.encodeSig sig
  , timestamp = ts
  }

-- | Sign a transaction with a private key
signTransaction
  :: Key.PrivateKey
  -> Transaction
  -> IO Key.Signature
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

hashInvalidTx :: InvalidTransaction -> Hash.Hash ByteString
hashInvalidTx (InvalidTransaction tx err) = hashTransaction tx

base16HashInvalidTx :: InvalidTransaction -> ByteString
base16HashInvalidTx = Encoding.base16 . Hash.getHash . hashInvalidTx

data InvalidTxField
  = InvalidTxTimestamp Time.Timestamp
  | InvalidTxSignature Key.InvalidSignature
  deriving (Show, Eq, Generic, S.Serialize)

data InvalidTxAccount
  = InvalidPubKeyByteString ByteString
  | RevokeValidatorError Address
  | AccountError Ledger.AccountError
  deriving (Show, Eq, Generic, S.Serialize)

data InvalidTxAsset
  = DerivedAddressesDontMatch Address Address
  | AssetError Ledger.AssetError
  deriving (Show, Eq, Generic, S.Serialize)

data InvalidTxContract
  = InvalidContract [Char]
  | ContractError Ledger.ContractError
  | MethodDoesNotExist Script.Name
  | InvalidCallArgType Script.Typecheck.TypeErrInfo
  | EvalFail Script.Eval.EvalFail
  deriving (Show, Eq, Generic, S.Serialize)

data InvalidTxHeader
  = InvalidTxAccount InvalidTxAccount
  | InvalidTxAsset InvalidTxAsset
  | InvalidTxContract InvalidTxContract
  deriving (Show, Eq, Generic, S.Serialize)

data TxValidationError
  = NoSuchOriginAccount Address
  | InvalidTxField InvalidTxField
  | InvalidTxHeader InvalidTxHeader
  | InvalidPubKey
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

data TxValidationCtx = TxBlock Time.Timestamp | TxMemPool | TxInvalid

-- | Validate a transaction without looking up the origin account
-- by using cryptography magic (public key recovery from signature)
validateTransaction
  :: TxValidationCtx
  -> Transaction
  -> IO (Either InvalidTransaction ())
validateTransaction txvctx tx@Transaction{..} = do
    isTimeValid <-
      case txvctx of
        TxBlock blockTs -> pure $
          Time.validateTimestamp_ blockTs timestamp
        TxMemPool ->
          Time.validateTimestamp timestamp
        -- For "validating" the data integrity of invalid txs
        TxInvalid -> pure True
    return $
      unless isTimeValid $
        Left $ mkInvalidTx $ InvalidTxTimestamp timestamp
 where
    mkInvalidTx = InvalidTransaction tx . InvalidTxField

-- | Validate a transaction without looking up the origin account
-- by using cryptography magic (public key recovery from signature).
-- This function does no timestamp validation, and used when validating
-- transactions from the database.
validateTransactionNoTs :: Transaction -> Either InvalidTransaction ()
validateTransactionNoTs tx@Transaction{..} = do
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
