{-|

LevelDB backend for ledger state on disk.

-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-type-patterns #-}


module DB.LevelDB (
  -- ** Low-level database
  LevelDB.DB,
  unDB,
  create,
  close,
  write,
  loadDBs,
  closeDBs,
  selectAll,

  -- ** High-level API
  LevelDBT(..),
  LevelDBM,
  runLevelDBT,

  -- ** Database
  Database(..),

  lockDb,
  unlockDb,

  -- ** Block persistence
  blockHeight,
  lastBlock,
  lastNBlocks,

  -- ** Sync World
  syncWorld,
  readWorld,

  -- ** Database (Directory) initialization
  setupDB,
  newDB,
  testDB,

  -- ** Danger
  resetDB,
  deleteDBs,
  deleteDBs_

) where

import Protolude

import Control.Arrow ((&&&))

import Account (Account)
import Asset (Asset)
import Contract (Contract)
import Address (Address)

import qualified Block
import qualified Transaction as Tx
import qualified Asset
import qualified Address
import qualified Account
import qualified Contract
import qualified Key
import qualified Storage
import qualified Ledger
import qualified Logging as Log

import Control.Monad.Base
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Distributed.Process.Lifted.Class

import DB.Class (MonadDB(..), MonadReadDB, MonadWriteDB)
import qualified DB.Class as DBC
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

newtype LevelDBT m a = LevelDBT
  { unLevelDBT :: ReaderT Database m a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadReader Database)

runLevelDBT :: Database -> LevelDBT m a -> m a
runLevelDBT dbs = flip runReaderT dbs . unLevelDBT

type LevelDBM = LevelDBT IO

--------------------------------------------------------------------------------
-- MonadBase/Control/TransControl Boilerplate
--------------------------------------------------------------------------------

instance MonadBase IO m => MonadBase IO (LevelDBT m) where
  liftBase = liftBaseDefault

instance MonadTransControl LevelDBT where
  type StT LevelDBT a = StT (ReaderT Database) a
  liftWith = defaultLiftWith LevelDBT unLevelDBT
  restoreT = defaultRestoreT LevelDBT

instance MonadBaseControl IO m => MonadBaseControl IO (LevelDBT m) where
  type StM (LevelDBT m) a = ComposeSt LevelDBT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM

instance MonadProcess m => MonadProcess (LevelDBT m) where
  liftP = LevelDBT . liftP

instance MonadProcessBase m => MonadProcessBase (LevelDBT m) where
  type StMP (LevelDBT m) a = ComposeStP LevelDBT m a
  liftBaseWithP = defaultLiftBaseWithP
  restoreMP = defaultRestoreMP


--------------------------------------------------------------------------------
-- DB.Class instances
--------------------------------------------------------------------------------

instance MonadBase IO m => MonadDB (LevelDBT m) where
  type Conn (LevelDBT m) = Database
  withConn f = do
    dbs <- ask
    liftBase $ f dbs

instance (MonadBase IO m) => MonadReadDB (LevelDBT m) where

  readAsset addr     = withConn (flip lookupDB addr . assetDB)
  readAssets         = withConn (selectAll . assetDB)

  readAccount addr   = withConn (flip lookupDB addr . accountDB)
  readAccounts       = withConn (selectAll . accountDB)

  readContract addr  = withConn (flip lookupDB addr . contractDB)
  readContracts      = withConn (selectAll . contractDB)

  readBlock index    = withConn (flip lookupDB index . blockDB)
  readLastBlock      = withConn (lastBlock . blockDB)
  readBlocks         = withConn (allBlocks . blockDB)
  readLastNBlocks n  = withConn (flip lastNBlocks n . blockDB)

  readInvalidTx hash = withConn (flip lookupDB hash . invalidTxDB)
  readInvalidTxs     = withConn (selectAll . invalidTxDB)

  readWorld          = withConn readWorld

instance (MonadBase IO m) => MonadWriteDB (LevelDBT m) where

  writeAsset asset     = withConn (flip writeDB asset . assetDB)
  writeAssets assets   = withConn (flip writeManyDB assets . assetDB)

  writeAccount acc     = withConn (flip writeDB acc . accountDB)
  writeAccounts accs   = withConn (flip writeManyDB accs . accountDB)

  writeContract c      = withConn (flip writeDB c . contractDB)
  writeContracts cs    = withConn (flip writeManyDB cs . contractDB)

  writeBlock blk       = withConn (flip writeDB blk . blockDB)
  writeBlocks blks     = withConn (flip writeManyDB blks . blockDB)

  writeInvalidTx itx   = withConn (flip writeDB itx . invalidTxDB)
  writeInvalidTxs itxs = withConn (flip writeManyDB itxs . invalidTxDB)

  resetDB              = withConn resetDB
  syncWorld world      = withConn (fmap Right . flip syncWorld world)

{-

.uplink/
  HEAD          -- World HEAD hash

  XXX Backend agnostic
  account       -- Node Account
  key           -- Node Account private key
  key.pub       -- Node Account public key
  peers         -- Node Peers
  txlog         -- Transaction log
  XXX

  contracts/    -- Contracts and local/global state
    contract0
    contract1
    ...
    contractN

  assets/       -- Assetss and holdings
    asset0
    asset1
    ...
    assetN

  accounts/     -- Accounts (currently addresses)
    acct0
    acct1
    ...
    acctN

  blocks/       -- Global transacction logs
    block0
    block1
    ...
    blockN

  invalidTxs/   -- All Transaction rejected by Node
    invalidTx0
    invalidTx1
    ...
    invalidTxN

-}

newtype AcctDB = AcctDB { unAcctDB :: LevelDB.DB }
newtype AssetDB = AssetDB { unAssetDB :: LevelDB.DB }
newtype ContractDB = ContractDB { unContractDB :: LevelDB.DB }
newtype BlockDB = BlockDB { unBlockDB :: LevelDB.DB }
newtype InvalidTxDB = InvalidTxDB { unInvalidTxDB :: LevelDB.DB }

-- | A type class for wrapping Level DBs and their paths
class LevelDB a where
  unDB   :: a -> LevelDB.DB
  dbPath :: a -> FilePath -> FilePath
  {-# MINIMAL unDB, dbPath #-}

instance LevelDB AcctDB where
  unDB (AcctDB db) = db
  dbPath _ = acctDir

instance LevelDB AssetDB where
  unDB (AssetDB db) = db
  dbPath _ = assetDir

instance LevelDB ContractDB where
  unDB (ContractDB db) = db
  dbPath _ = contractDir

instance LevelDB BlockDB where
  unDB (BlockDB db) = db
  dbPath _ = blockDir

instance LevelDB InvalidTxDB  where
  unDB (InvalidTxDB db) = db
  dbPath _ = invalidTxDir

-- | Associates a value with a wrapped DB type
--   As long as the programmer takes care that DB b is
--   really storing values of type a, this works
class (Show (Key a), LevelDB db) => HasDB a db | a -> db where
  type Key a
  encodeValue :: a -> ByteString
  decodeValue :: ByteString -> Either Text a
  getKey      :: a -> Key a
  validate    :: a -> IO Bool
  {-# MINIMAL encodeValue, decodeValue, validate, getKey #-}

instance HasDB Account.Account AcctDB where
  type Key Account.Account = Address
  encodeValue = S.encode
  decodeValue = first toS . S.decode
  getKey      = Account.address
  validate    = pure . Account.validateAccount

instance HasDB Asset.Asset AssetDB where
  type Key Asset.Asset = Address
  encodeValue = S.encode
  decodeValue = first toS . S.decode
  getKey      = Asset.address
  validate = Asset.validateAsset

instance HasDB Block.Block BlockDB where
  type Key Block.Block = Int
  encodeValue = S.encode
  decodeValue = first toS . S.decode
  getKey      = Block.index
  validate = pure . Block.validBlockDB

instance HasDB Contract.Contract ContractDB where
  type Key Contract = Address
  encodeValue = S.encode
  decodeValue = first toS . S.decode
  getKey      = Contract.address
  validate    = Contract.validateContract

instance HasDB Tx.InvalidTransaction InvalidTxDB where
  type Key Tx.InvalidTransaction = ByteString
  encodeValue = S.encode
  decodeValue = first toS . S.decode
  getKey      = Tx.base16HashInvalidTx
  validate (Tx.InvalidTransaction tx err) =
    either (const False) (const False) <$>
      Tx.validateTransaction tx

-- | Database cursor references
data Database = Database
  { root        :: FilePath
  , accountDB   :: AcctDB
  , assetDB     :: AssetDB
  , blockDB     :: BlockDB
  , contractDB  :: ContractDB
  , invalidTxDB :: InvalidTxDB
  }

-------------------------------------------------------------------------------
-- Generic Database functions
-------------------------------------------------------------------------------

lookupDB :: (HasDB a db) => db -> Key a -> IO (Either Text a)
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

writeDB :: (HasDB a db) => db -> a -> IO ()
writeDB db val =
  LevelDB.put
    (unDB db)
    LevelDB.defaultWriteOptions
    (show $ getKey val)
    (encodeValue val)

writeManyDB :: (HasDB a db) => db -> [a] -> IO ()
writeManyDB db = mapM_ (writeDB db)

parseAllValues
  :: (ByteString -> Either Text a)
  -> [(ByteString,ByteString)]
  -> Either Text [a]
parseAllValues f = sequence . fmap (f . snd)

-- | Generic version
selectAll :: (HasDB a b) => b -> IO (Either Text [a])
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

loadDBs :: FilePath -> IO Database
loadDBs dbpath = do
  acctDB      <- AcctDB      <$> create (acctDir dbpath)
  assetDB     <- AssetDB     <$> create (assetDir dbpath)
  blockDB     <- BlockDB     <$> create (blockDir dbpath)
  contractDB  <- ContractDB  <$> create (contractDir dbpath)
  invalidTxDB <- InvalidTxDB <$> create (invalidTxDir dbpath)
  return Database
    { root        = dbpath
    , accountDB   = acctDB
    , assetDB     = assetDB
    , blockDB     = blockDB
    , contractDB  = contractDB
    , invalidTxDB = invalidTxDB
    }

closeDBs :: Database -> IO ()
closeDBs (Database _ accDB assDB blkDB ctrDB itxDB) = do
  close $ unDB accDB
  close $ unDB assDB
  close $ unDB blkDB
  close $ unDB ctrDB
  close $ unDB itxDB

-------------------------------------------------------------------------------
-- Block Database
-------------------------------------------------------------------------------

-- | Query Block height
blockHeight :: BlockDB -> IO (Either Text Int)
blockHeight = fmap (fmap length) . allBlocks

lastBlock :: BlockDB -> IO (Either Text Block.Block)
lastBlock db = do
  eIdx <- blockHeight db
  case eIdx of
    Left err -> pure $ Left err
    Right idx -> lookupDB db (idx - 1)

-- | Query the last n blocks, may return < n blocks if
-- chain contains fewer blocks than n
lastNBlocks :: BlockDB -> Int -> IO (Either Text [Block.Block])
lastNBlocks db n
  | n < 0     = pure $ Left "lastNBlocks: n must be a positive number."
  | otherwise = fmap (take n . reverse) <$> allBlocks db

-- | Read all blocks (sorted)
allBlocks :: BlockDB -> IO (Either Text [Block.Block])
allBlocks = fmap (fmap Block.sortBlocks) . selectAll

-------------------------------------------------------------------------------
-- Sync World
-------------------------------------------------------------------------------

newtype TaggedBatchOps a = TaggedBatchOps { unTaggedBatchOps :: [LevelDB.BatchOp] }
  deriving (Monoid)

-- | Sync world state to database, atomically*.
syncWorld :: Database -> Ledger.World -> IO ()
syncWorld db world = do
  deletes <- deleteBatchOps db
  writes  <- writeBatchOps world
  let (syncAccs, syncAssets, syncContracts) = deletes <> writes
  -- Atomically updates each DB with new
  write (unDB $ accountDB db) $ unTaggedBatchOps syncAccs
  write (unDB $ assetDB db) $ unTaggedBatchOps syncAssets
  write (unDB $ contractDB db) $ unTaggedBatchOps syncContracts

-- | Write world to database persistence layer.
writeWorld :: Database -> Ledger.World -> IO ()
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
readWorld :: Database -> IO (Either Text Ledger.World)
readWorld Database{..} = do
  liftA3 Ledger.mkWorld
    <$> selectAll assetDB
    <*> selectAll accountDB
    <*> selectAll contractDB

deleteWorld :: Database -> IO ()
deleteWorld dbs = do
  (delAccs, delAssets, delContracts) <- deleteBatchOps dbs
  LevelDB.write (unDB $ accountDB dbs) LevelDB.defaultWriteOptions $ unTaggedBatchOps delAccs
  LevelDB.write (unDB $ assetDB dbs) LevelDB.defaultWriteOptions $ unTaggedBatchOps delAssets
  LevelDB.write (unDB $ contractDB dbs) LevelDB.defaultWriteOptions $ unTaggedBatchOps delContracts

deleteBatchOps
  :: Database
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

invalidTxDir :: FilePath -> FilePath
invalidTxDir root = root </> "invalidTxs"

currentFile :: FilePath
currentFile = "CURRENT"

lockingFile :: FilePath
lockingFile = "LOCK"

-- | Setup a folder as a database or load an existing database. Locks the
-- database so that other processes cannot read or write from/to the database.
setupDB :: FilePath -> IO (Either Text Database)
setupDB dbpath = do
  rootExists <- doesDirectoryExist dbpath
  if rootExists then do
    db <- loadDBs dbpath
    integrity <- testDB dbpath
    if integrity then do
      Log.info $ "Using existing database at: " <> toS dbpath
      lockDb dbpath
      pure (Right db)
    else pure $ Left $
      "Database in " <> show dbpath <> " is not well-formed."
  else do
    db <- newDB dbpath
    lockDb (root db)
    pure $ Right db

-- | Setup a folder as a database
-- Warning: Does not lock the database
newDB :: FilePath -> IO Database
newDB root = do

  -- Setup directories
  createDirectoryIfMissing True root
  createDirectoryIfMissing True (blockDir root)
  createDirectoryIfMissing True (contractDir root)
  createDirectoryIfMissing True (assetDir root)
  createDirectoryIfMissing True (acctDir root)
  createDirectoryIfMissing True (invalidTxDir root)

  -- Setup files
  writeFile (root </> "HEAD") mempty

  -- Setup databases
  loadDBs root

-- | Wipes entire existing DB without prompt
-- Warning: Wipes current DB with no prompting and no exception handling
resetDB
  :: Database
  -> IO (Either Text ())
resetDB db = do
  dbExists <- doesDirectoryExist $ root db
  if dbExists
    then do
      closeDBs db
      deleteDBs_ db
      newDB $ root db
      pure $ Right ()
    else do
      pure $ Left $
        "resetDB: DB does not exist at " <> show (root db)
  where

-- | Destroy all the databases and wipes the directory that was
-- used as the database (used during resetDB).
deleteDBs :: Database -> IO ()
deleteDBs db = do
  closeDBs db
  removeDirectoryRecursive (root db)

-- | Destroy all the databases (used during resetDB)
deleteDBs_ :: Database -> IO ()
deleteDBs_ db = do
    destroy' (accountDB db)
    destroy' (assetDB db)
    destroy' (contractDB db)
    destroy' (blockDB db)
    destroy' (invalidTxDB db)
  where
    destroy' :: LevelDB a => a -> IO ()
    destroy' = flip LevelDB.destroy LevelDB.defaultOptions . flip dbPath (root db)

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
  , doesDirectoryExist (invalidTxDir root)

   -- Check files
  , doesFileExist (root </> "HEAD")

   -- Check databases
  , doesFileExist (blockDir root     </> currentFile)
  , doesFileExist (contractDir root  </> currentFile)
  , doesFileExist (assetDir root     </> currentFile)
  , doesFileExist (acctDir root      </> currentFile)
  , doesFileExist (invalidTxDir root </> currentFile)
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
