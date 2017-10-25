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
  TxAsset(..),
  TxContract(..),
  TxAccount(..),
  TransactionHeader(..),
  SyncLocalOp(..),

  -- ** Creation
  newTransaction,

  -- ** Hashing
  hashTransaction,

  -- ** Serialization
  encodeTransaction,
  decodeTransaction,

  -- ** Validation / Verification
  verifyTransaction,
  validateTransaction,

  InvalidTransaction(..),
  InvalidTxHeader(..),
  InvalidTxAccount(..),
  InvalidTxAsset(..),
  InvalidTxContract(..),
  TxValidationError(..),
  InvalidTxField(..),

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
import Datetime.Types
import Asset (Asset, Balance, putAssetType, getAssetType, putRef, getRef)
import Address (Address)
import Utils (toInt, toWord16)
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

-------------------------------------------------------------------------------
-- Transaction
-------------------------------------------------------------------------------

-- | Transaction
data Transaction = Transaction
  { header    :: TransactionHeader
  , signature :: ByteString
  , origin    :: Address
  , to        :: Maybe Address
  , timestamp :: Time.Timestamp
  } deriving (Show, Eq, Generic, NFData, Serialize, Hash.Hashable)

-- | Transaction header
data TransactionHeader
  = TxContract TxContract  -- ^ Contract transaction
  | TxAsset TxAsset        -- ^ Asset transactions
  | TxAccount TxAccount    -- ^ Account transactions
  deriving (Show, Eq, Generic, NFData, Hash.Hashable)

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
      assetName :: SafeString.SafeString      -- ^ Asset name
    , supply    :: Int64                      -- ^ Asset supply
    , reference :: Maybe Asset.Ref            -- ^ Asset reference
    , assetType :: Asset.AssetType            -- ^ Asset type
  }

  | Transfer {
      assetAddr :: Address                    -- ^ Address of asset
    , toAddr    :: Address                    -- ^ Asset of Receiver
    , balance   :: Balance                    -- ^ Amount to transfer
  }

  | Bind {
      assetAddr    :: Address                 -- ^ Asset address
    , contractAddr :: Address                 -- ^ Account address
    , bindProof    :: (Integer, Integer)      -- ^ Bind proof
  }
  deriving (Show, Eq, Generic, NFData, Hash.Hashable)

data TxAccount
  = CreateAccount {
      pubKey   :: ByteString                  -- ^ Public key associated with the account
    , timezone :: ByteString                  -- ^ Time zone
    , metadata :: Map ByteString ByteString   -- ^ Arbitrary additional metadata
  }
  | RevokeAccount {
      address   :: Address                    -- ^ Issue a revocation of an account
  }
  deriving (Show, Eq, Generic, NFData, Hash.Hashable)

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

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
    if | code == 1000 || code == 1001 || code == 1002 ->
          getTxContract code'

       | code == 1003 || code == 1004 || code == 1005 ->
          getTxAsset code'

       | code == 1006 || code == 1007 ->
          getTxAccount code'

       | otherwise -> fail $ "Unknown code: " <> show code


getTxContract :: Int -> Get TransactionHeader
getTxContract 1000 = do
  addr     <- Address.getAddress
  contract <- get
  pure $ TxContract $ CreateContract addr contract

getTxContract 1002 = do
  address <- Address.getAddress
  method  <- get
  args    <- get
  pure $ TxContract $ Call address method args
getTxContract n = fail $ "getTxContract " <> show n

getTxAsset :: Int -> Get TransactionHeader
getTxAsset 1003 = do
  name <- SafeString.getSafeString
  supply <- getInt64be
  ref <- getWord16be
  mRef <- do
    if | ref == 0 -> return Nothing
       | ref == 1 -> Just <$> getRef
       | True     -> fail $ show ref <> " not a valid CreateAsset reference prefix."

  assetType <- getAssetType

  pure $ TxAsset $ CreateAsset name supply mRef assetType

getTxAsset 1004 = do
  asset <- Address.getAddress
  to    <- Address.getAddress
  bal   <- getInt64be
  pure $ TxAsset $ Transfer asset to bal

getTxAsset n = fail $ "getTxAsset " <> show n


getTxAccount :: Int -> Get TransactionHeader
getTxAccount 1006 = do
  pubKeyLen <- getWord16be
  pubKey <- getBytes $ (toInt pubKeyLen :: Int)
  tzLen <- getWord16be
  tz <- getBytes $ toInt tzLen
  md <- getMap

  pure $ TxAccount $ CreateAccount pubKey tz md
getTxAccount 1007 = do
  addr <- Address.getAddress
  pure $ TxAccount $ RevokeAccount addr
getTxAccount n = fail $ "getTxAccount " <> show n

instance Serialize TxContract where
  put txc = case txc of
    CreateContract addr contract -> do
      putWord16be 1000
      Address.putAddress addr
      put contract

    SyncLocal addr op -> do
      putWord16be 1001
      Address.putAddress addr
      put op



    Call address method args -> do
      putWord16be 1002
      Address.putAddress address
      put method
      put args

instance Serialize TxAsset where
  put txa = case txa of
    CreateAsset name supply mRef assetType -> do
      putWord16be 1003
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
      Address.putAddress asset
      Address.putAddress to
      putInt64be bal

    Bind asset act (r,s) -> do
      putWord16be 1005
      Address.putAddress asset
      Address.putAddress act
      Key.putSignature (Key.mkSignatureRS (r,s))

instance Serialize TxAccount where
  put txa = case txa of
    CreateAccount pubKey tz md -> do
      putWord16be 1006
      putWord16be $ toWord16 $ BS.length pubKey
      putByteString pubKey
      putWord16be $ toWord16 $ BS.length tz
      putByteString tz
      putMap md

    RevokeAccount addr -> do
      putWord16be 1007
      Address.putAddress addr

instance Serialize SyncLocalOp where
  put op = case op of
        (InitialCommit (a, b)) -> do
          put a
          put b
        _ -> undefined

txCall :: Address -> ByteString -> [Storage.Value] -> TxContract
txCall = Call

-------------------------------------------------------------------------------
-- Map Serialization
-------------------------------------------------------------------------------

getMap :: Get (Map ByteString ByteString)
getMap = do
  len <- getWord16be
  go [] 0 len
  where
    go acc i len
      | i == len = return $ Map.fromList acc
      | otherwise = do
        keyLen <- getWord16be
        key    <- getBytes $ toInt keyLen
        valLen <- getWord16be
        val    <- getBytes $ toInt valLen
        go ((key, val) : acc) (i+1) len

putMap :: Map ByteString ByteString -> PutM ()
putMap m = do
  let len = Map.size m
  putWord16be $ toWord16 len
  go $ sortBy (\a b -> compare (fst a) (fst b)) $ Map.toList m

  where
    go [] = return ()
    go ((k,v):xs) = do
      putWord16be $ toWord16 $ BS.length k
      putByteString k
      putWord16be $ toWord16 $ BS.length v
      putByteString v
      go xs

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
        , "to"        .= to t
        ]

instance FromJSON Transaction where
  parseJSON (Object v) = do
    hd     <- v .: "header"
    sig    <- v .: "signature"
    origin <- v .: "origin"
    to     <- v .:? "to"
    ts     <- v .: "timestamp"
    pure $ Transaction
      { header    = hd
      , signature = encodeUtf8 sig
      , origin    = origin
      , to        = to
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
    ,  "address"  .= contractAddr
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
  toJSON (CreateAsset name supply ref assetType) = object -- XXX
    [ "tag"      .= ("CreateAsset" :: Text)
    , "contents" .= object
        [ "assetName" .= (toS $ SafeString.toBytes name :: [Char])
        , "supply"    .= supply
        , "reference" .= ref
        , "assetType" .= Asset.jsonType assetType
        ]
    ]
  toJSON (Bind asset con sig) = object
    [ "tag"      .= ("Bind" :: Text)
    , "contents" .= object
      [ "asset"    .= asset
      , "contract" .= con
      , "proof"    .= (toS (Key.hexRS sig) :: Text)
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

instance FromJSON TxAsset where
  parseJSON (Object v) = do
    tagV <- v .: "tag" :: Parser [Char]
    if | tagV == "CreateAsset" -> do
          c <- v .: "contents"
          CreateAsset <$> c .: "assetName"
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
      , "metadata"   .= (Map.fromList $ map (\(k,v) -> (decodeUtf8 k , decodeUtf8 v)) $ Map.toList md)
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
        let md' = Map.fromList $ map (\(k, v) -> (encodeUtf8 k, encodeUtf8 v)) $ Map.toList md
            pk' = encodeUtf8 pk
            tz' = encodeUtf8 tz
        pure $ CreateAccount pk' tz' md'
      "RevokeAccount" -> RevokeAccount <$> contents .: "address"
      _ -> fail $ "Unknown tag in TxAccount: " <> tagV

  parseJSON invalid = typeMismatch "TxAccount" invalid

-- | Serialize transaction
encodeTransaction :: Transaction -> ByteString
encodeTransaction = encode

-- | Deserialize transaction
decodeTransaction :: ByteString -> Either [Char] Transaction
decodeTransaction = decode

-- | Hash transaction header
hashTransaction :: Transaction -> ByteString
hashTransaction = hashTransactionHeader . header

-- | Hash transaction header
hashTransactionHeader :: TransactionHeader -> ByteString
hashTransactionHeader = Encoding.base16 . Hash.getHash . Hash.toHash

-------------------------------------------------------------------------------
-- Creation
-------------------------------------------------------------------------------

-- | Create a new transaction.
newTransaction
  :: Address            -- ^ Origin Account Address
  -> Maybe Address      -- ^ Computed Target Address
  -> Key.PrivateKey     -- ^ Private Key
  -> TransactionHeader  -- ^ Transaction payload
  -> IO Transaction
newTransaction origin to privKey header = do
  ts     <- Time.now
  sig    <- Key.sign privKey $ S.encode header
  pure $ Transaction {
    header    = header
  , origin    = origin
  , to        = to
  , signature = Key.encodeSig sig
  , timestamp = ts
  }

-- | Sign a transaction with a private key
signTransaction
  :: Key.PrivateKey
  -> Transaction
  -> IO Key.Signature
signTransaction key = Key.sign key . S.encode . header

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

-------------------------------------------------------------------------------
-- Validation / Verification
-------------------------------------------------------------------------------

{-

Validates a transaction without respect to a Ledger State

* Timestamp is valid
* Signature is valid

-}

data InvalidTxField
  = InvalidTxTimestamp Time.Timestamp
  | InvalidTxSignature Key.InvalidSignature
  deriving (Show, Eq)

data InvalidTxAccount
  = InvalidPubKeyByteString Text
  | RevokeValidatorError Address
  | AccountError Ledger.AccountError
  deriving (Show, Eq)

data InvalidTxAsset
  = MissingAssetAddress
  | AssetError Ledger.AssetError
  deriving (Show, Eq)

data InvalidTxContract
  = InvalidContract Text
  | ContractError Ledger.ContractError
  | MethodDoesNotExist Text
  | InvalidCallArgType Script.Typecheck.TypeErrInfo
  | EvalFail Script.Eval.EvalFail
  deriving (Show, Eq)

data InvalidTxHeader
  = InvalidTxAccount InvalidTxAccount
  | InvalidTxAsset InvalidTxAsset
  | InvalidTxContract InvalidTxContract
  | NotImplemented Text
  deriving (Show, Eq)

data TxValidationError
  = NoSuchOriginAccount Address
  | InvalidTxField InvalidTxField
  | InvalidTxHeader InvalidTxHeader
  | InvalidPubKey
  deriving (Show, Eq)

data InvalidTransaction
  = InvalidTransaction Transaction TxValidationError
  deriving (Show, Eq)

-- | Validate a transaction without looking up the origin account
-- by using cryptography magic (public key recovery from signature)
validateTransaction :: Transaction -> IO (Either InvalidTransaction ())
validateTransaction tx@Transaction{..} = do
    isTimeValid <- Time.validateTimestamp timestamp
    return $ do
      -- Validate transaction timestamp
      unless isTimeValid $
        Left $ mkInvalidTx $ InvalidTxTimestamp timestamp
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
