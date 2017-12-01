{-|

Account data structures and serialization.

--}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveAnyClass #-}

module Account (
  -- ** Types
  Account(..),
  Metadata(..),
  newAccount,
  createAccount,
  createAccountDir,
  validateAccount,

  -- ** Serialization
  encodeAccount,
  decodeAccount,
  accountKeyVal,

  -- ** Save/Load Account
  saveAccount,
  loadAccountFile,

  -- ** Persistence
  AccountPrompt(..),
  setupAccount,
  setupAccount',
  loadAccount,
  loadAccount',

  readKeys,
  writeKeys,
  writeAccount,
  writeAccountData,
  readAccountData

) where

import Protolude
import qualified Key
import qualified Hash
import qualified Utils
import qualified Address
import qualified Logging as Log
import Data.Aeson ((.=), (.:))
import Data.Aeson.Types (typeMismatch)
import qualified Data.Map as Map
import qualified Data.Aeson as A
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Binary as Binary
import qualified Text.PrettyPrint.Leijen.Text as PP

import Control.Monad (fail)

import Database.PostgreSQL.Simple.ToRow     (ToRow(..))
import Database.PostgreSQL.Simple.FromRow   (FromRow(..), field)
import Database.PostgreSQL.Simple.ToField   (ToField(..), Action(..))
import Database.PostgreSQL.Simple.FromField (FromField(..), ResultError(..), returnError)

import System.FilePath
import System.Directory
import System.Posix.Files

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- XXX: needs to not have unbounded bytestring objects for metadata
-- Need a Safe ByteString

-- | An account in a distributed legder consists of:
--
-- 1. An elliptic public key
-- 2. An address
-- 3. A node key of the screen hosting the account.
-- 4. A ISO timezone code
-- 5. An arbitray key-value metadata store of personal information (name,
-- company, geography, etc)
newtype Metadata = Metadata
  { unMetadata :: Map ByteString ByteString }
  deriving (Show, Eq, Generic, NFData, Hash.Hashable)

instance Monoid Metadata where
  mempty = Metadata mempty
  (Metadata m1) `mappend` (Metadata m2) =
    Metadata $ m1 <> m2

data Account = Account
  { publicKey   :: Key.PubKey
  , address     :: Address.Address
  , timezone    :: ByteString
  , metadata    :: Metadata
  } deriving (Show, Eq, Generic, NFData, S.Serialize, Hash.Hashable)

-- | Validate the integrity of acccount information
validateAccount :: Account -> Bool
validateAccount Account {..} = and [
    Key.validateKey publicKey
  , Address.validateAddress address
  , Address.verifyAddress publicKey address
  ]

-- | Create a new Acccount
createAccount
  :: Key.PubKey
  -> ByteString
  -> Metadata
  -> Account
createAccount pub timezone metadata =
    Account {
      publicKey  = pub
    , address    = Address.deriveAddress pub
    , timezone   = timezone
    , metadata   = metadata
    }

-- | Generate a new Acccount (random seed)
-- returns private key so the new account can be written to disk
newAccount :: ByteString -> Metadata -> IO (Account, Key.ECDSAKeyPair)
newAccount timezone metadata = do
  keys@(pub,_) <- Key.new
  return $ (,keys) $
    createAccount pub timezone metadata

-- | Single user key file settings
pubKeyFile, privKeyFile, accountFile :: FilePath -> FilePath
pubKeyFile root  = root </> "key.pub"
privKeyFile root = root </> "key"
accountFile root = root </> "account"

accountFilesExist :: FilePath -> IO Bool
accountFilesExist root = do
  pubKeyExists  <- doesFileExist $ pubKeyFile root
  privKeyExists <- doesFileExist $ privKeyFile root
  accountExists <- doesFileExist $ accountFile root
  return $ pubKeyExists && privKeyExists && accountExists

loadAccount :: FilePath -> IO (Either Text (Account, Key.ECDSAKeyPair))
loadAccount root = do
  accountExists <- accountFilesExist root
  if accountExists then loadAccount' root
  else return $ Left $ "Could not find account at path: " <> toS root

-- | Like `loadAccount` but will throw an exception if account doesn't exist
loadAccount' :: FilePath -> IO (Either Text (Account, Key.ECDSAKeyPair))
loadAccount' root = do
  res <- liftA3 (,,)
    <$> Utils.safeRead (pubKeyFile root)
    <*> Utils.safeRead (privKeyFile root)
    <*> Utils.safeRead (accountFile root)

  case res of
    Left err -> return $ Left err
    (Right (pubKeyPem, privKeyPem, account)) ->
      pure $ do
        pk   <- Key.importPub pubKeyPem
        pair <- Key.importPriv privKeyPem
        acc  <- first toS $ A.eitherDecode' (toS account)
        if (Key.validatePair pair) && (pk == publicKey acc)
          then pure (acc, pair)
          else (Left "key.pub and account don't match keys")

data AccountPrompt = Prompt | NoPrompt
  deriving (Eq, Ord, Show)

-- | Used when spinning up a new node via cmd line.
-- This function checks for an existing account and loads it.
-- Otherwise it prompts user for account creation.
setupAccount
  :: FilePath
  -> Maybe FilePath
  -> AccountPrompt
  -> IO (Either Text (Account, Key.ECDSAKeyPair))
setupAccount root privKey accPrompt = do
    eAcc <- loadAccount root
    case eAcc of
      Left err -> do
        ePrivKey <- case privKey of
          Nothing -> promptSupplyKey $ promptArg accPrompt
          Just _  -> promptKeyFile privKey
        case ePrivKey of
          Left err -> return $ Left err
          Right privKey -> do
            -- XXX Prompt for timezone & metadata? How?
            let pubKey = Key.toPublic privKey
                acc = createAccount pubKey "UTC" mempty
                keys = (pubKey,privKey)
            Log.info "Writing account data to disk."
            writeAccountData root acc keys
            return $ Right (acc, keys)
      Right acc -> return $ Right acc
  where
    promptArg Prompt = Nothing
    promptArg NoPrompt = Just "n"

    -- Prompt the user to supply a private key or not
    promptSupplyKey :: Maybe Text -> IO (Either Text Key.PrivateKey)
    promptSupplyKey (Just "y") = promptKeyFile Nothing
    promptSupplyKey (Just "n") = Right . snd <$> Key.new
    promptSupplyKey (Just _)   = do
      yOrNo <- Utils.prompt "Please enter 'y' or 'n': "
      promptSupplyKey $ Just yOrNo
    promptSupplyKey Nothing    = do
      yOrNo <- Utils.prompt "Do you want to supply your own private key (y/n): "
      promptSupplyKey $ Just yOrNo

    -- Prompt the user for a path to file containing their hex or pem priv key
    promptKeyFile :: Maybe FilePath -> IO (Either Text Key.PrivateKey)
    promptKeyFile (Just privKeyFile) = do
      privKeyFp <- makeAbsolute privKeyFile
      fileExists <- doesFileExist privKeyFp
      if fileExists then do
        privKeyBS <- BS.readFile =<< makeAbsolute privKeyFile
        return $ Key.tryDecodePriv privKeyBS
      else do
        putText $ "Could not find file at filepath " <> show privKeyFp <> "."
        promptKeyFile Nothing
    promptKeyFile Nothing = do
      privKeyFile <- Utils.prompt "Please provide the filepath to your EC private key: "
      promptKeyFile $ Just $ toS privKeyFile

-- | Like `setupAccount`, but will throw an exception if an error occurs.
setupAccount' :: FilePath -> Maybe FilePath -> AccountPrompt -> IO (Account, Key.ECDSAKeyPair)
setupAccount' root privKey accPrompt = do
  eAcc <- Account.setupAccount root privKey accPrompt
  case eAcc of
    Left err  -> Utils.dieRed err
    Right acc -> return acc

createAccountDir :: FilePath -> IO (Either Text ())
createAccountDir root = do
  rootExists <- doesDirectoryExist root
  if rootExists then
    pure (Left "Not overwriting existing keys.")
  else do
    createDirectoryIfMissing True root
    pure (Right ())

readKeys :: FilePath -> IO (Either Text Key.ECDSAKeyPair)
readKeys root = do
  rootExists <- doesDirectoryExist root
  if rootExists then do
    let privFile = privKeyFile root
    privExists <- doesFileExist privFile
    if privExists then do
      privKeyPem <- BS.readFile privFile
      pure $ Key.importPriv privKeyPem
    else
      pure $ Left "One of the public or private keys does not exist"
  else
    pure $ Left "Node root directory has not been initialized."

-- | Write account key data to disk
writeKeys :: FilePath -> (Key.PubKey, Key.PrivateKey) -> IO (Either Text ())
writeKeys root (publicKey, privateKey) = do
  rootExists <- doesDirectoryExist root
  if rootExists then do
    let pubFile  = pubKeyFile root
    let privFile = privKeyFile root
    pubExists  <- doesFileExist pubFile
    privExists <- doesFileExist privFile
    if (pubExists || privExists) then
      pure (Left "Not overwriting existing keys.")
    else do
      BS.writeFile privFile (Key.exportPriv (publicKey, privateKey))
      BS.writeFile pubFile (Key.exportPub publicKey)
      -- Set chmod 0400
      setFileMode pubFile ownerReadMode
      setFileMode privFile ownerReadMode
      pure (Right ())
  else
    pure (Left "Root directory does not exist.")

-- | Write account metadata to disk
writeAccount :: FilePath -> Account -> IO (Either Text ())
writeAccount root acct = do
  rootExists <- doesDirectoryExist root
  if rootExists then do
    let acctFile = accountFile root
    acctExists <- doesFileExist acctFile
    if acctExists then
      pure (Left "Not overwriting existing account file.")
    else do
      BS.writeFile acctFile (toS (A.encodePretty acct))
      setFileMode acctFile ownerReadMode
      pure (Right ())
  else
    pure (Left "Root directory does not exist.")

writeAccountData
  :: FilePath
  -> Account
  -> (Key.PubKey, Key.PrivateKey)
  -> IO (Either Text ())
writeAccountData root acc keys = do
  eRes <- writeAccount root acc
  case eRes of
    Left err -> pure $ Left err
    Right _  -> writeKeys root keys

readAccountData
  :: FilePath
  -> IO (Either Text (Account, Key.ECDSAKeyPair))
readAccountData root = loadAccount root

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

instance A.ToJSON Account where
  toJSON acc = A.object
    [ "publicKey" .= decodeUtf8 (Key.unHexPub (Key.hexPub (publicKey acc)))
    , "address"   .= address acc
    , "timezone"  .= decodeUtf8 (timezone acc)
    , "metadata"  .= metadata acc
    ]

instance A.FromJSON Account where
  parseJSON o@(A.Object v) = do
    pubHex   <- v .: "publicKey"
    addr     <- v .: "address"
    timezone <- v .: "timezone"
    metadata <- v .: "metadata"

    publicKey <- case Key.dehexPub (encodeUtf8 pubHex) of
      Left err -> fail $ "FromJSON Account.publicKey: " <> err
      Right k -> pure k

    pure $ Account
      publicKey
      addr
      (encodeUtf8 timezone)
      (parseMetadata metadata)

  parseJSON invalid = typeMismatch "Account" invalid

instance Binary.Binary Account where
  put tx = Binary.put $ S.encode tx
  get = do
    bs <- Binary.get
    case S.decode bs of
      (Right tx) -> return tx
      (Left err) -> fail err

instance S.Serialize Metadata where
  put (Metadata m) = do
    let len = Map.size m
    S.putWord16be $ Utils.toWord16 len
    go $ sortBy (\a b -> compare (fst a) (fst b)) $ Map.toList m

    where
      go [] = return ()
      go ((k,v):xs) = do
        S.putWord16be $ Utils.toWord16 $ BS.length k
        S.putByteString k
        S.putWord16be $ Utils.toWord16 $ BS.length v
        S.putByteString v
        go xs
  get = do
      len <- S.getWord16be
      go [] 0 len
    where
      go acc i len
        | i == len = return $ Metadata $ Map.fromList acc
        | otherwise = do
          keyLen <- S.getWord16be
          key    <- S.getBytes $ Utils.toInt keyLen
          valLen <- S.getWord16be
          val    <- S.getBytes $ Utils.toInt valLen
          go ((key, val) : acc) (i+1) len

instance A.ToJSON Metadata where
  toJSON (Metadata metadata) = A.toJSON $
    map decodeUtf8 $ Map.mapKeys decodeUtf8 metadata

instance A.FromJSON Metadata where
  parseJSON = fmap parseMetadata . A.parseJSON

parseMetadata :: Map Text Text -> Metadata
parseMetadata keys = Metadata $ map encodeUtf8 (Map.mapKeys encodeUtf8 $ keys)

-- | Binary serialize account
encodeAccount :: Account -> ByteString
encodeAccount = S.encode

-- | Binary deserialize account
decodeAccount :: ByteString -> Either [Char] Account
decodeAccount = S.decode

accountKeyVal :: Account -> (Address.Address, Account)
accountKeyVal acct = (address acct, acct)

loadAccountFile :: FilePath -> IO (Either [Char] Account)
loadAccountFile fp = BS.readFile fp >>= return . A.eitherDecodeStrict

saveAccount :: Account -> FilePath -> IO ()
saveAccount acc fp = BSL.writeFile fp $ A.encodePretty acc

-------------------------------------------------------------------------------
-- Postgres DB
-------------------------------------------------------------------------------

instance ToField Metadata where
  toField = EscapeByteA . S.encode . Map.toList . unMetadata

instance FromField Metadata where
  fromField f mdata = do
    bs <- fromField f mdata
    case fmap Map.fromList . S.decode <$> bs of
      Nothing               -> returnError UnexpectedNull f ""
      Just (Left err)       -> returnError ConversionFailed f err
      Just (Right metadata) -> return $ Metadata metadata

instance FromRow Account
instance ToRow Account
