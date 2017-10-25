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
  Metadata,
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
type Metadata = Map ByteString ByteString

data Account = Account
  { publicKey   :: Key.PubKey
  , address     :: Address.Address
  , nodeKey     :: Integer
  , timezone    :: ByteString
  , metadata    :: Metadata
  } deriving (Show, Eq, Generic, NFData, S.Serialize, Hash.Hashable)

instance A.ToJSON Account where
  toJSON acc = A.object
    [ "publicKey" .= decodeUtf8 (Key.unHexPub (Key.hexPub (publicKey acc)))
    , "address"   .= address acc
    , "nodeKey"   .= nodeKey acc
    , "timezone"  .= decodeUtf8 (timezone acc)
    , "metadata"  .= map decodeUtf8 (Map.mapKeys decodeUtf8 $ metadata acc)
    ]

instance A.FromJSON Account where
  parseJSON o@(A.Object v) = do
    pubHex   <- v .: "publicKey"
    addrRaw  <- v .: "address"
    nodeKey  <- v .: "nodeKey"
    timezone <- v .: "timezone"
    metadata <- v .: "metadata"

    publicKey <- case Key.dehexPub (encodeUtf8 pubHex) of
      Left err -> fail $ "FromJSON Account.publicKey: " <> err
      Right k -> pure k

    pure $ Account
      publicKey
      (Address.fromRaw (encodeUtf8 addrRaw))
      nodeKey
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

parseMetadata :: Map Text Text -> Map ByteString ByteString
parseMetadata keys = map encodeUtf8 (Map.mapKeys encodeUtf8 $ keys)

-- | Validate the integrity of acccount information
validateAccount :: Account -> Bool
validateAccount Account {..} = and [
    Key.validateKey publicKey
  , Address.validateAddress address
  , Address.verifyAddress publicKey address
  , nodeKey > 0x0
  ]

-- | Create a new Acccount
createAccount
  :: Key.PubKey
  -> ByteString
  -> Map ByteString ByteString
  -> Account
createAccount pub timezone metadata =
    Account {
      publicKey  = pub
    , address    = Address.deriveAddress pub
    , nodeKey    = 0x1
    , timezone   = timezone
    , metadata   = metadata
    }

-- | Generate a new Acccount (random seed)
-- returns private key so the new account can be written to disk
newAccount :: ByteString -> Map ByteString ByteString -> IO (Account, Key.ECDSAKeyPair)
newAccount timezone metadata = do
  keys@(pub,_) <- Key.new
  return $ (,keys) $
    createAccount pub timezone metadata

-- | Single user key file settings
pubKeyFile, privKeyFile, accountFile :: FilePath
pubKeyFile = "key.pub"
privKeyFile = "key"
accountFile = "account"

accountFilesExist :: FilePath -> IO Bool
accountFilesExist root = do
  pubKeyExists  <- doesFileExist $ root </> pubKeyFile
  privKeyExists <- doesFileExist $ root </> privKeyFile
  accountExists <- doesFileExist $ root </> accountFile
  return $ pubKeyExists && privKeyExists && accountExists

loadAccount :: FilePath -> IO (Either Text Account)
loadAccount root = do
  accountExists <- accountFilesExist root
  if accountExists then loadAccount' root
  else return $ Left $ "Could not find account at path: " <> toS root

-- | Like `loadAccount` but will throw an exception if account doesn't exist
loadAccount' :: FilePath -> IO (Either Text Account)
loadAccount' root = do
  res <- liftA3 (,,) 
    <$> Utils.safeRead (root </> pubKeyFile)
    <*> Utils.safeRead (root </> privKeyFile)
    <*> Utils.safeRead (root </> accountFile)

  case res of
    Left err -> return $ Left err
    (Right (pubKeyPem, privKeyPem, account)) -> do
      let
        process = do
          pk   <- Key.importPub pubKeyPem
          pair <- Key.importPriv privKeyPem
          acc  <- first toS $ A.eitherDecode' (toS account)
          if (Key.validatePair pair) && (pk == publicKey acc)
            then pure (pk, pair, acc)
            else (Left "key.pub and account don't match keys")

      case process of
        Left err            -> pure (Left err)
        Right (pk, sk, acc) -> pure (Right acc)

data AccountPrompt = Prompt | NoPrompt
  deriving (Eq, Ord, Show)

-- | Used when spinning up a new node via cmd line.
-- This function checks for an existing account and loads it.
-- Otherwise it prompts user for account creation.
setupAccount :: FilePath -> Maybe FilePath -> AccountPrompt -> IO (Either Text Account)
setupAccount root privKey accPrompt = do
    eAcc <- loadAccount root
    case eAcc of
      Left err -> do
        Log.info "Creating a new account."

        ePrivKey <- case privKey of
                      Nothing -> promptSupplyKey $ promptArg accPrompt
                      (Just _) -> promptKeyFile privKey
        case ePrivKey of
          Left err -> return $ Left err
          Right privKey -> do
            -- XXX Prompt for timezone & metadata? How?
            let pubKey = Key.toPublic privKey
                acc = createAccount pubKey "UTC" mempty
                keys = (pubKey,privKey)
            writeKeys root keys
            writeAccount root acc
            return $ Right acc
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
setupAccount' :: FilePath -> (Maybe FilePath) -> AccountPrompt -> IO Account
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
    let privFile = root </> privKeyFile
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
    let pubFile  = root </> pubKeyFile
    let privFile = root </> privKeyFile
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
    let acctFile = root </> accountFile
    acctExists <- doesFileExist acctFile
    if acctExists then
      pure (Left "Not overwriting existing account file.")
    else do
      BS.writeFile acctFile (toS (A.encodePretty acct))
      setFileMode acctFile ownerReadMode
      pure (Right ())
  else
    pure (Left "Root directory does not exist.")

----------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

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



