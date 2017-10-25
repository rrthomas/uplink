{-|

LevelDB backend for ledger state on disk.

-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DB (
  -- ** Low-level database
  LevelDB.DB,
  unDB,
  create,
  close,
  write,
  loadDBs,
  closeDBs,
  selectAll,

  -- ** Databases
  AcctDB(..),
  AssetDB(..),
  ContractDB(..),
  BlockDB(..),
  Databases(..),

  blockDir,
  acctDir,
  assetDir,
  contractDir,
  storageDir,

  lockDb,
  unlockDb,

  -- ** Block persistence
  allBlocks,
  writeBlock,
  writeBlocks,
  lookupBlock,
  latestBlock,
  blockHeight,
  lastNBlocks,

  -- ** Address persistence
  allAccounts,
  writeAccount,
  writeAccounts,
  lookupAccount,

  -- ** Asset persistence
  allAssets,
  allAssetsWithAddrs,
  writeAsset,
  writeAssets,
  lookupAsset,

  -- ** Contract persistence
  allContracts,
  writeContractWithAddr,
  writeContract,
  writeContracts,
  lookupContract,

  -- ** Global transaction log
  txLogFile,

  -- ** Sync World
  syncWorld,
  readWorld,

  -- ** Folder initialization
  setupNewDB,
  setupExistingDB,
  testDB,

) where

import Protolude

import Account (Account)
import Asset (Asset)
import Contract (Contract)
import Address (Address)

import qualified Block
import qualified Asset
import qualified Address
import qualified Account
import qualified Contract
import qualified Storage
import qualified Ledger
import qualified Logging as Log

import System.FilePath
import System.FileLock
import System.Directory
import System.Posix.Files

import Data.List (unzip)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Database.LevelDB.Base as LevelDB
import qualified Database.LevelDB.Internal as DBInternal (unsafeClose)
import qualified Data.Serialize as S

{-

.uplink/
  HEAD          -- World HEAD hash
  account       -- Account.hs

  txlog         -- Transaction log

  contracts/    -- Contracts and local/global state
    px0
    px1

  assets/       -- Assetss and holdings
    ax0
    ax1

  accounts/     -- Accounts (currently addresses)
    acct0
    acct1

  blocks/       -- Global transacction logs
    block0
    block1
    block2

-}

newtype AcctDB = AcctDB { unAcctDB :: LevelDB.DB }
newtype AssetDB = AssetDB { unAssetDB :: LevelDB.DB }
newtype ContractDB = ContractDB { unContractDB :: LevelDB.DB }
newtype BlockDB = BlockDB { unBlockDB :: LevelDB.DB }

-- | A wrapped DB type class
class LevelDB a where
  unDB :: a -> LevelDB.DB
  {-# MINIMAL unDB #-}

instance LevelDB AcctDB where
  unDB (AcctDB db) = db

instance LevelDB AssetDB where
  unDB (AssetDB db) = db

instance LevelDB ContractDB where
  unDB (ContractDB db) = db

instance LevelDB BlockDB where
  unDB (BlockDB db) = db

-- | Associates a value with a wrapped DB type
--   As long as the programmer takes care that DB b is
--   really storing values of type a, this works
class (Show (Key a), LevelDB db) => HasDB a db | a -> db where
  type Key a
  encodeValue :: a -> ByteString
  decodeValue :: ByteString -> Either [Char] a
  validate :: a -> IO Bool
  {-# MINIMAL encodeValue, decodeValue, validate #-}

-- | XXX Move to respective files
instance HasDB Account.Account AcctDB where
  type Key Account.Account = Address
  encodeValue = S.encode
  decodeValue = S.decode
  validate = pure . Account.validateAccount

instance HasDB Asset.Asset AssetDB where
  type Key Asset.Asset = Address
  encodeValue = S.encode
  decodeValue = S.decode
  validate = Asset.validateAsset

instance HasDB Block.Block BlockDB where
  type Key Block.Block = Int
  encodeValue = S.encode
  decodeValue = S.decode
  validate = Block.validBlockDB

instance HasDB Contract.Contract ContractDB where
  type Key Contract = Address
  encodeValue = S.encode
  decodeValue = S.decode
  validate = Contract.validateContract

-- | Database cursor references
data Databases = Databases
  { accountDB  :: AcctDB
  , assetDB    :: AssetDB
  , blockDB    :: BlockDB
  , contractDB :: ContractDB
  }

-------------------------------------------------------------------------------
-- Generic Database functions
-------------------------------------------------------------------------------

lookupDB :: (HasDB a db) => db -> Key a -> IO (Either [Char] a)
lookupDB db key = do
  mValBS <- lookup (unDB db) $ show key
  case mValBS of
    Nothing -> pure $ Left $
      "lookupDB: No value with key " <> show key <> " found."
    Just valBS -> case decodeValue valBS of
      Left err -> pure $ Left err
      Right val -> do
        isValid <- validate val
        if isValid then
          pure $ Right val
        else pure $ Left $
          "lookupDB: Value with key " <> show key <> " malformed."

writeDB :: (HasDB a db) => db -> (Key a, a) -> IO ()
writeDB db (key, val) = LevelDB.put (unDB db) LevelDB.defaultWriteOptions (show key) $ encodeValue val

writeManyDB :: (HasDB a db) => db -> [(Key a, a)] -> IO ()
writeManyDB db = mapM_ (writeDB db)

parseAllValues
  :: (ByteString -> Either [Char] a)
  -> [(ByteString,ByteString)]
  -> Either [Char] [a]
parseAllValues f = sequence . fmap (f . snd)

-- | Generic version
selectAll :: (HasDB a b) => b -> IO (Either [Char] [a])
selectAll = fmap parseAllValues' . selectAllBS
  where parseAllValues' = sequence . fmap (decodeValue . snd)

-- | XXX: this doesn't handle failure gracefully
selectAllBS :: LevelDB a => a -> IO [(ByteString, ByteString)]
selectAllBS db = do
    iter <- LevelDB.createIter (unDB db) $ LevelDB.ReadOptions False True Nothing
    LevelDB.iterFirst iter
    vals <- go iter True []
    LevelDB.releaseIter iter
    return vals
  where
    go iter valid !acc = do
      val   <- LevelDB.iterEntry iter
      valid <- LevelDB.iterValid iter
      LevelDB.iterNext iter
      case (valid, val) of
        (True, Just (k,v)) -> go iter valid ((k,v) : acc)
        _                  -> return acc

-------------------------------------------------------------------------------
-- Database Setup
-------------------------------------------------------------------------------

-- | Initialize a LevelDB database.
create :: FilePath -> IO LevelDB.DB
create dbPath = LevelDB.open dbPath (LevelDB.defaultOptions { LevelDB.createIfMissing = True } )

{-# NOINLINE close #-}
-- | Close a LevelDB handler.
close :: LevelDB.DB -> IO ()
close = DBInternal.unsafeClose

-- | Write raw bytes to a key value pair in LevelDB.
put :: LevelDB.DB -> ByteString -> ByteString -> IO ()
put db = LevelDB.put db LevelDB.defaultWriteOptions

write :: LevelDB.DB -> LevelDB.WriteBatch -> IO ()
write db = LevelDB.write db LevelDB.defaultWriteOptions

-- | Lookup raw bytess from LevelDB.
lookup :: LevelDB.DB -> ByteString -> IO (Maybe ByteString)
lookup db = LevelDB.get db LevelDB.defaultReadOptions

loadDBs :: FilePath -> IO Databases
loadDBs dbpath = do
  acctDB     <- AcctDB     <$> create (acctDir dbpath)
  assetDB    <- AssetDB    <$> create (assetDir dbpath)
  blockDB    <- BlockDB    <$> create (blockDir dbpath)
  contractDB <- ContractDB <$> create (contractDir dbpath)
  return Databases
    { accountDB  = acctDB
    , assetDB    = assetDB
    , blockDB    = blockDB
    , contractDB = contractDB
    }

closeDBs :: Databases -> IO ()
closeDBs (Databases accDB assDB blkDB ctrDB) = do
  close $ unDB accDB
  close $ unDB assDB
  close $ unDB blkDB
  close $ unDB ctrDB

-------------------------------------------------------------------------------
-- Block Database
-------------------------------------------------------------------------------

-- | Write a block to disk.
writeBlock :: BlockDB -> Block.Block -> IO ()
writeBlock db = writeDB db . Block.blockKeyVal

-- | Write a sequence of blocks to disk.
writeBlocks :: BlockDB -> [Block.Block] -> IO ()
writeBlocks db = mapM_ (writeBlock db)

-- | Query Block height
blockHeight :: BlockDB -> IO (Either [Char] Int)
blockHeight db = do
  blocks <- allBlocks db
  let blks = Block.sortBlocks <$> blocks
  return (length <$> blks)

latestBlock :: BlockDB -> IO (Either [Char] Block.Block)
latestBlock db = do
  eIdx <- blockHeight db
  case eIdx of
    Left err -> pure $ Left err
    Right idx -> lookupBlock db (idx - 1)

-- | Query the last n blocks, may return < n blocks if
-- chain contains fewer blocks than n
lastNBlocks :: BlockDB -> Int -> IO (Either [Char] [Block.Block])
lastNBlocks db n
  | n < 0     = pure $ Left "lastNBlocks: n must be a positive number."
  | otherwise = fmap (take n . sortBy (flip compare)) <$> allBlocks db

-- | Lookup a block
lookupBlock :: BlockDB -> Int -> IO (Either [Char] Block.Block)
lookupBlock = lookupDB

-- | Read all blocks.
allBlocks :: BlockDB -> IO (Either [Char] [Block.Block])
allBlocks = selectAll

-------------------------------------------------------------------------------
-- Account Database
-------------------------------------------------------------------------------

writeAccount :: AcctDB -> Account.Account -> IO ()
writeAccount db = writeDB db . Account.accountKeyVal

writeAccounts :: AcctDB -> [Account.Account] -> IO ()
writeAccounts db = mapM_ (writeAccount db)

lookupAccount :: AcctDB -> Address -> IO (Either [Char] Account.Account)
lookupAccount = lookupDB

allAccounts :: AcctDB -> IO (Either [Char] [Account.Account])
allAccounts = selectAll

-------------------------------------------------------------------------------
-- Asset Database (Cannot derive HasDB because Address derivation needs PrivKey)
-------------------------------------------------------------------------------

-- | Write an asset to disk.
writeAsset :: AssetDB -> (Address, Asset.Asset) -> IO ()
writeAsset = writeDB

-- | Write a sequence of assets to disk.
writeAssets :: AssetDB -> [(Address, Asset.Asset)] -> IO ()
writeAssets db = mapM_ (writeAsset db)

-- | Read all Assets
allAssets :: AssetDB -> IO (Either [Char] [Asset.Asset])
allAssets = selectAll

allAssetsWithAddrs :: AssetDB -> IO (Either [Char] [(Address, Asset.Asset)])
allAssetsWithAddrs db = do
    (bsAddrs,bsAssets) <- unzip <$> selectAllBS db
    let addrs = map Address.fromRaw bsAddrs
        assets = map Asset.decodeAsset bsAssets
        addrsAndAssets = zipWith matchAddrToAsset addrs assets
    return $ sequence addrsAndAssets
  where
    matchAddrToAsset addr eAsset = fmap (addr,) eAsset

-- | Lookup an Asset
lookupAsset :: AssetDB -> Address -> IO (Either [Char] Asset.Asset)
lookupAsset = lookupDB

-------------------------------------------------------------------------------
-- Contract Database
-------------------------------------------------------------------------------

writeContract :: ContractDB -> Contract -> IO ()
writeContract db = writeDB db . contractKeyVal

writeContracts :: ContractDB -> [Contract] -> IO ()
writeContracts db = mapM_ (writeContract db)

writeContractWithAddr :: ContractDB -> Contract -> Address -> IO ()
writeContractWithAddr db c addr = writeDB db (addr, c)

-- | Read all contracts.
allContracts :: ContractDB -> IO (Either [Char] [Contract])
allContracts = selectAll

-- | Lookup a contract.
lookupContract :: ContractDB -> Address -> IO (Either [Char] Contract)
lookupContract = lookupDB

contractKeyVal :: Contract -> (Address, Contract)
contractKeyVal contract = (Contract.address contract, contract)

-------------------------------------------------------------------------------
-- Sync World
-------------------------------------------------------------------------------

newtype TaggedBatchOps a = TaggedBatchOps { unTaggedBatchOps :: [LevelDB.BatchOp] }
  deriving (Monoid)

-- | Sync world state to database, atomically*.
-- XXX Use [BatchOp] and `write` from LevelDB to do this atomically
syncWorld :: Databases -> Ledger.World -> IO ()
syncWorld dbs world = do
  deletes <- deleteBatchOps dbs
  writes  <- writeBatchOps world
  let (syncAccs, syncAssets, syncContracts) = deletes <> writes
  -- Atomically updates each DB with new
  write (unDB $ accountDB dbs) $ unTaggedBatchOps syncAccs
  write (unDB $ assetDB dbs) $ unTaggedBatchOps syncAssets
  write (unDB $ contractDB dbs) $ unTaggedBatchOps syncContracts

-- | Write world to database persistence layer.
writeWorld :: Databases -> Ledger.World -> IO ()
writeWorld dbs world = do
  (putAccs, putAssets, putContracts) <- writeBatchOps world
  -- writeAccounts
  write (unDB $ accountDB dbs) $ unTaggedBatchOps putAccs
  -- writeAssets
  write (unDB $ assetDB dbs) $ unTaggedBatchOps putAssets
  -- writeContracts
  write (unDB $ contractDB dbs) $ unTaggedBatchOps putContracts

writeBatchOps
  :: Ledger.World
  -> IO (TaggedBatchOps Account, TaggedBatchOps Asset, TaggedBatchOps Contract)
writeBatchOps world = do
    let putAccs = keysToPuts $ Map.toList $ Ledger.accounts world
    let putAssets = keysToPuts $ Map.toList $ Ledger.assets world
    let putContracts = keysToPuts $ Map.toList $ Ledger.contracts world
    pure (putAccs, putAssets, putContracts)
  where
    keysToPuts :: HasDB a db => [(Address,a)] -> TaggedBatchOps a
    keysToPuts = TaggedBatchOps . map (uncurry LevelDB.Put . bimap show encodeValue)

-- | Load world state from database.
readWorld :: Databases -> IO (Either [Char] Ledger.World)
readWorld Databases{..} = do
  eAccts <- allAccounts accountDB
  eAssetsAndAddrs <- allAssetsWithAddrs assetDB
  eContracts <- allContracts contractDB
  pure $ Ledger.World
    <$> fmap (Map.fromList . map contractKeyVal) eContracts
    <*> fmap Map.fromList eAssetsAndAddrs
    <*> fmap (Map.fromList . map Account.accountKeyVal) eAccts

deleteWorld :: Databases -> IO ()
deleteWorld dbs = do
  (delAccs, delAssets, delContracts) <- deleteBatchOps dbs
  LevelDB.write (unDB $ accountDB dbs) LevelDB.defaultWriteOptions $ unTaggedBatchOps delAccs
  LevelDB.write (unDB $ assetDB dbs) LevelDB.defaultWriteOptions $ unTaggedBatchOps delAssets
  LevelDB.write (unDB $ contractDB dbs) LevelDB.defaultWriteOptions $ unTaggedBatchOps delContracts

deleteBatchOps
  :: Databases
  -> IO (TaggedBatchOps Account, TaggedBatchOps Asset, TaggedBatchOps Contract)
deleteBatchOps dbs = do
    -- Read current world
    eWorld <- readWorld dbs
    case eWorld of
      Left err -> do
        Log.critical (toS err)
        panic (toS err)
      Right world -> do
        -- construct BatchOp Del operations
        let accountsDel  = keysToPut $ Ledger.accounts world
        let assetsDel    = keysToPut $ Ledger.assets world
        let contractsDel = keysToPut $ Ledger.contracts world
        pure (accountsDel, assetsDel, contractsDel)
  where
    keysToPut = TaggedBatchOps . map (LevelDB.Del . show) . Map.keys


-------------------------------------------------------------------------------
-- File System
-------------------------------------------------------------------------------

blockDir :: FilePath -> FilePath
blockDir root = root </> "blocks"

contractDir :: FilePath ->  FilePath
contractDir root = root </> "contracts"

assetDir :: FilePath ->  FilePath
assetDir root = root </> "assets"

acctDir :: FilePath ->  FilePath
acctDir root = root </> "accounts"

storageDir :: FilePath -> FilePath
storageDir root = root </> "storage"

txLogFile :: FilePath -> FilePath
txLogFile root = root </> "txlog"

currentFile :: FilePath
currentFile = "CURRENT"

lockingFile :: FilePath
lockingFile = "LOCK"

data DBSetup = New | Existing

setupNewDB :: FilePath -> IO (Either Text FilePath)
setupNewDB = setupDB New

setupExistingDB :: FilePath -> IO (Either Text FilePath)
setupExistingDB = setupDB Existing

-- | Setup a folder as a database.
setupDB :: DBSetup -> FilePath -> IO (Either Text FilePath)
setupDB dbSetup root = do
  rootExists <- doesDirectoryExist root
  case dbSetup of
    Existing -> do
      if rootExists then do
        integrity <- testDB root
        if integrity then do
          Log.info $ "Using existing database at: " <> toS root
          lockDb root
          pure (Right root) -- Existing database is good.
        else pure $ Left $ "Database in " <> show root <> " is not well-formed."
      else Right <$> newDB root
    New -> do
      if rootExists then do
        shouldOverwrite <- promptOverwrite Nothing
        if shouldOverwrite then do
          removeDirectoryRecursive root
          Right <$> newDB root
        else setupDB Existing root
      else Right <$> newDB root
  where
    promptOverwrite :: Maybe Text -> IO Bool
    promptOverwrite (Just "y") = return True
    promptOverwrite (Just "n") = return False
    promptOverwrite (Just _) = do
      putStr ("Please enter 'y' or 'n': " :: Text)
      yesOrNo <- getLine
      promptOverwrite (Just yesOrNo)
    promptOverwrite Nothing = do
      putText $ "An uplink DB already exists in this directory."
      putStr ("Do you want to overwrite it? (y/n): " :: Text)
      yesOrNo <- getLine
      promptOverwrite $ Just yesOrNo

newDB :: FilePath -> IO FilePath
newDB root = do
  putText $ "Creating new database at: " <> toS root

  -- Setup diretories
  createDirectoryIfMissing True root
  createDirectoryIfMissing True (blockDir root)
  createDirectoryIfMissing True (contractDir root)
  createDirectoryIfMissing True (assetDir root)
  createDirectoryIfMissing True (acctDir root)
  createDirectoryIfMissing True (storageDir root)

  -- Setup files
  writeFile (root </> "HEAD") mempty
  writeFile (txLogFile root) mempty

  -- Setup databases
  create (blockDir root)    >>= close
  create (contractDir root) >>= close
  create (assetDir root)    >>= close
  create (acctDir root)     >>= close
  create (storageDir root)  >>= close

  -- Create lockfile
  lockDb root
  pure root

-- LevelDB database consists of CURRENT and LOG

-- | Test the integrity of an existing folder structure for containing a
-- well-formed database.
testDB :: FilePath -> IO Bool
testDB root = and <$> sequence [
   -- Check directory structure
    doesDirectoryExist root
  , doesDirectoryExist (blockDir root)
  , doesDirectoryExist (contractDir root)
  , doesDirectoryExist (assetDir root)
  , doesDirectoryExist (acctDir root)
  , doesDirectoryExist (storageDir root)

   -- Check files
  , doesFileExist (root </> "HEAD")

   -- Check databases
  , doesFileExist (blockDir root    </> currentFile)
  , doesFileExist (contractDir root </> currentFile)
  , doesFileExist (assetDir root    </> currentFile)
  , doesFileExist (acctDir root     </> currentFile)
  , doesFileExist (storageDir root  </> currentFile)
  ]

-------------------------------------------------------------------------------
-- Locking
-------------------------------------------------------------------------------

lockDb :: FilePath -> IO ()
lockDb root = do
  let lockfile = (root </> lockingFile)
  lockExists <- doesFileExist lockfile
  if lockExists
    then do
      acquired <- tryLockFile lockfile Exclusive
      case acquired of
        Nothing -> die "Cannot open database. It is locked by another process."
        Just _  -> pass
    else do
      writeFile lockfile mempty
      lockFile lockfile Exclusive
      return ()

unlockDb :: FilePath -> IO ()
unlockDb root = do
  let lockfile = (root </> lockingFile)
  acquired <- tryLockFile lockfile Exclusive
  case acquired of
    Nothing   -> die "Cannot unlock database. It is locked by another process."
    Just lock -> unlockFile lock
