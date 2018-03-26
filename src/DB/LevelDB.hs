{-|

LevelDB backend for ledger state on disk.

-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -fno-warn-unused-type-patterns #-}


module DB.LevelDB (
  -- ** Low-level database
  LevelDB.DB,
  unDB,
  create,
  close,
  write,
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
  createDB,
  loadExistingDB,
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

import System.IO.Error

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
  type DBConn (LevelDBT m) = Database
  type DBError (LevelDBT m) = LevelDBError
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

  readTransaction h  = withConn (flip lookupDB h . txDB)

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

  writeBlock blk       = withConn (flip writeBlock blk . (blockDB &&& txDB))
  writeBlocks blks     = withConn (flip writeBlocks blks . (blockDB &&& txDB))

  writeInvalidTx itx   = withConn (flip writeDB itx . invalidTxDB)
  writeInvalidTxs itxs = withConn (flip writeManyDB itxs . invalidTxDB)

  resetDB              = withConn resetDB
  syncWorld world      = withConn (flip syncWorld world)

{-

.uplink/

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

  blocks/       -- Global transaction log by block index
    block0
    block1
    ...
    blockN

  txs/          -- Global transaction log by hash
    hashTx1
    hashTx2
    ...
    hashTxN

  invalidTxs/   -- All Transaction rejected by Node
    invalidTx0
    invalidTx1
    ...
    invalidTxN

-}

newtype AcctDB      = AcctDB { unAcctDB :: LevelDB.DB }
newtype AssetDB     = AssetDB { unAssetDB :: LevelDB.DB }
newtype ContractDB  = ContractDB { unContractDB :: LevelDB.DB }
newtype BlockDB     = BlockDB { unBlockDB :: LevelDB.DB }
newtype TxDB        = TxDB { unTxDB :: LevelDB.DB }
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

instance LevelDB TxDB where
  unDB (TxDB db) = db
  dbPath _ = txDir

instance LevelDB InvalidTxDB  where
  unDB (InvalidTxDB db) = db
  dbPath _ = invalidTxDir

-- | Associates a value with a wrapped DB type
--   As long as the programmer takes care that DB b is
--   really storing values of type a, this works
class (Show (Key a), LevelDB db) => HasDB a db | a -> db where
  type Key a

  encodeValue :: a -> ByteString
  default encodeValue :: S.Serialize a => a -> ByteString
  encodeValue = S.encode

  decodeValue :: ByteString -> Either Text a
  default decodeValue :: S.Serialize a => ByteString -> Either Text a
  decodeValue = first toS . S.decode

  getKey      :: a -> Key a
  validate    :: a -> Bool
  {-# MINIMAL validate, getKey #-}

instance HasDB Account.Account AcctDB where
  type Key Account.Account = Address
  getKey   = Account.address
  validate = Account.validateAccount

instance HasDB Asset.Asset AssetDB where
  type Key Asset.Asset = Address
  getKey   = Asset.address
  validate = Asset.validateAsset

instance HasDB Block.Block BlockDB where
  type Key Block.Block = Int
  getKey   = Block.index
  validate = Block.validateBlockDB

instance HasDB Contract.Contract ContractDB where
  type Key Contract = Address
  getKey   = Contract.address
  validate = Contract.validateContract

instance HasDB Tx.Transaction TxDB where
  type Key Tx.Transaction = ByteString
  getKey = Tx.base16HashTransaction
  validate tx =
    either (const False) (const True) $
      Tx.validateTransaction tx

instance HasDB Tx.InvalidTransaction InvalidTxDB where
  type Key Tx.InvalidTransaction = ByteString
  getKey = Tx.base16HashInvalidTx
  validate (Tx.InvalidTransaction tx err) =
    either (const False) (const True) $
      Tx.validateTransaction tx

-- | Database cursor references
data Database = Database
  { root        :: FilePath
  , accountDB   :: AcctDB
  , assetDB     :: AssetDB
  , blockDB     :: BlockDB
  , contractDB  :: ContractDB
  , txDB        :: TxDB
  , invalidTxDB :: InvalidTxDB
  }

-------------------------------------------------------------------------------
-- LevelDB Errors
-------------------------------------------------------------------------------

data LevelDBError
  = ReadFail Text
  | WriteFail Text
  | IterError Text
  | InvalidValueAtKey (Text,Text)
  | MalformedValueAtKey Text Text
  | MalformedDatabase Text
  | DBDoesNotExist Text
  | LevelDBError Text
  deriving (Show, Eq, Ord)

instance Exception LevelDBError

toWriteFailErr :: Show a => Either a b -> Either LevelDBError b
toWriteFailErr = first (WriteFail . show)

-------------------------------------------------------------------------------
-- Generic Database functions
-------------------------------------------------------------------------------

tryDB :: IO a -> IO (Either IOError a)
tryDB action = try action

lookupDB
  :: (Show a, HasDB a db)
  => db
  -> Key a
  -> IO (Either LevelDBError a)
lookupDB db key = do
  mValBS <- lookup (unDB db) $ show key
  case mValBS of
    Nothing -> pure $ Left $ ReadFail $ show key
    Just valBS -> case decodeValue valBS of
      Left err -> pure $ Left $
        MalformedValueAtKey (show key) err
      Right val
        | validate val -> pure $ Right val
        | otherwise -> pure $ Left $ InvalidValueAtKey (show key,show val)

writeDB :: (HasDB a db) => db -> a -> IO (Either LevelDBError ())
writeDB db val =
  fmap toWriteFailErr $
    tryDB $
      LevelDB.put
        (unDB db)
        LevelDB.defaultWriteOptions
        (show $ getKey val)
        (encodeValue val)

-- | Writes values in a batch operation (transaction). If this function fails,
-- no values will be written to the database.
writeManyDB :: (HasDB a db) => db -> [a] -> IO (Either LevelDBError ())
writeManyDB db vals =
    fmap toWriteFailErr $
      tryDB $
        LevelDB.write
          (unDB db)
          LevelDB.defaultWriteOptions
          (map mkPut vals)
  where
    mkPut v = LevelDB.Put (show $ getKey v) (encodeValue v)

-- | Parses all key/value pairs of ByteStrings into Haskell values
parseAllValues
  :: (HasDB a db)
  => [(ByteString,ByteString)]
  -> Either LevelDBError [a]
parseAllValues = sequence . map parseValue
  where
    parseValue (k,v) =
      first (MalformedValueAtKey $ show k) $
        decodeValue v

-- | Generic version
selectAll
  :: (HasDB a db)
  => db
  -> IO (Either LevelDBError [a])
selectAll db = do
  eAllBSs <- selectAllBS db
  pure $ parseAllValues =<< eAllBSs

selectAllBS
  :: LevelDB db
  => db
  -> IO (Either LevelDBError [(ByteString, ByteString)])
selectAllBS db =
    fmap toIterErr $ tryDB $ do
      iter <- LevelDB.createIter (unDB db) $ LevelDB.ReadOptions False True Nothing
      LevelDB.iterFirst iter
      vals <- go iter True []
      LevelDB.releaseIter iter
      return vals
  where
    toIterErr = first (IterError . show)

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

-- | Setup a directory as a LevelDB database. The directory must _not_ already exist.
-- Locks the database so that other processes cannot read or write from/to the database.
createDB :: FilePath -> IO (Either Text Database)
createDB root = do
  dirExists <- doesDirectoryExist root
  if dirExists
     then pure $ Left $ "Directory '" <> toS root <> "' already exists. Not overwriting."
     else do
       createDirectoryIfMissing True root
       db <- createDB' root
       lockDb root
       pure $ Right db

-- | Setup a directory as a LevelDB database. The directory must already exist.
-- Locks the database so that other processes cannot read or write from/to the database.
createDB' :: FilePath -> IO Database
createDB' root = do
  acctDB      <- AcctDB      <$> create (acctDir root)
  assetDB     <- AssetDB     <$> create (assetDir root)
  blockDB     <- BlockDB     <$> create (blockDir root)
  contractDB  <- ContractDB  <$> create (contractDir root)
  txDB        <- TxDB        <$> create (txDir root)
  invalidTxDB <- InvalidTxDB <$> create (invalidTxDir root)
  return Database
    { root        = root
    , accountDB   = acctDB
    , assetDB     = assetDB
    , blockDB     = blockDB
    , contractDB  = contractDB
    , txDB        = txDB
    , invalidTxDB = invalidTxDB
    }

-- | Try to load an existing LevelDB Database from a directory. Locks the
-- database so that other processes cannot read or write from/to the database.
loadExistingDB :: FilePath ->  IO (Either Text Database)
loadExistingDB root = do
  rootExists <- doesDirectoryExist root
  if rootExists
     then do
       db <- createDB' root
       -- Test the DB was created successfully
       integrity <- testDB root
       if integrity
          then do
            lockDb root
            pure $ Right db
          else pure $ Left $
            "Database in " <> toS root <> " is not well-formed."
     else pure $ Left $
       "Database does not exist at path " <> toS root <> "."

-- | Close the DBs, removing the validity of all DB handles
closeDBs :: Database -> IO ()
closeDBs (Database _ accDB assDB blkDB ctrDB txDB itxDB) = do
  close $ unDB accDB
  close $ unDB assDB
  close $ unDB blkDB
  close $ unDB ctrDB
  close $ unDB txDB
  close $ unDB itxDB

-------------------------------------------------------------------------------
-- Block Database
-------------------------------------------------------------------------------

-- | Query Block height
blockHeight :: BlockDB -> IO (Either LevelDBError Int)
blockHeight = fmap (fmap length) . allBlocks

lastBlock :: BlockDB -> IO (Either LevelDBError Block.Block)
lastBlock db = do
  eIdx <- blockHeight db
  case eIdx of
    Left err -> pure $ Left err
    Right idx -> lookupDB db (idx - 1)

-- | Query the last n blocks, may return < n blocks if
-- chain contains fewer blocks than n
lastNBlocks :: BlockDB -> Int -> IO (Either LevelDBError [Block.Block])
lastNBlocks db n
  | n < 0     = pure $ Left $
      LevelDBError "lastNBlocks: n must be a positive number."
  | otherwise = fmap (take n . reverse) <$> allBlocks db

-- | Read all blocks (sorted)
allBlocks :: BlockDB -> IO (Either LevelDBError [Block.Block])
allBlocks = fmap (fmap Block.sortBlocks) . selectAll

-- | Writes a block and transactions to their respective dbs
writeBlock :: (BlockDB, TxDB) -> Block.Block -> IO (Either LevelDBError ())
writeBlock (BlockDB bdb, TxDB txdb) blk =
    fmap toWriteFailErr $ tryDB $ do
      write bdb [mkPut blk]
      write txdb txPuts
  where
    mkPut v = LevelDB.Put (show $ getKey v) (encodeValue v)
    txPuts    = map mkPut (Block.transactions blk)

writeBlocks :: (BlockDB, TxDB) -> [Block.Block] -> IO (Either LevelDBError ())
writeBlocks (BlockDB bdb, TxDB txdb) blks =
   fmap toWriteFailErr $ tryDB $ do
      write bdb blkPuts
      write txdb txPuts
 where
    mkPut v = LevelDB.Put (show $ getKey v) (encodeValue v)
    blkPuts = map mkPut blks
    txPuts  = concatMap (map mkPut . Block.transactions) blks

-------------------------------------------------------------------------------
-- Sync World
-------------------------------------------------------------------------------

newtype TaggedBatchOps a = TaggedBatchOps { unTaggedBatchOps :: [LevelDB.BatchOp] }
  deriving (Monoid)

-- | Sync world state to database, atomically*.
syncWorld :: Database -> Ledger.World -> IO (Either LevelDBError ())
syncWorld db world =
    fmap toWriteErr $ tryDB $ do
      deletes <- mkDeleteBatchOps db
      writes  <- mkWriteBatchOps world
      let (syncAccs, syncAssets, syncContracts) = deletes <> writes
      -- Atomically updates each DB with new
      write (unDB $ accountDB db) $ unTaggedBatchOps syncAccs
      write (unDB $ assetDB db) $ unTaggedBatchOps syncAssets
      write (unDB $ contractDB db) $ unTaggedBatchOps syncContracts
  where
    toWriteErr = first (WriteFail . show)

-- | Write world to database persistence layer.
writeWorld :: Database -> Ledger.World -> IO ()
writeWorld dbs world = do
    (putAccs, putAssets, putContracts) <- mkWriteBatchOps world
    write' (accountDB dbs)  putAccs
    write' (assetDB dbs)    putAssets
    write' (contractDB dbs) putContracts
  where
    write' db toDel =
      LevelDB.write (unDB db) LevelDB.defaultWriteOptions (unTaggedBatchOps toDel)

mkWriteBatchOps
  :: Ledger.World
  -> IO (TaggedBatchOps Account, TaggedBatchOps Asset, TaggedBatchOps Contract)
mkWriteBatchOps world = do
    let putAccs = keysToPuts $ Map.toList $ Ledger.accounts world
    let putAssets = keysToPuts $ Map.toList $ Ledger.assets world
    let putContracts = keysToPuts $ Map.toList $ Ledger.contracts world
    pure (putAccs, putAssets, putContracts)
  where
    keysToPuts :: HasDB a db => [(Address,a)] -> TaggedBatchOps a
    keysToPuts = TaggedBatchOps . map (uncurry LevelDB.Put . bimap show encodeValue)

-- | Load world state from database.
readWorld :: Database -> IO (Either LevelDBError Ledger.World)
readWorld Database{..} = do
  liftA3 Ledger.mkWorld
    <$> selectAll assetDB
    <*> selectAll accountDB
    <*> selectAll contractDB

-- | Delete the entire world. This function cannot be batched,
-- because BatchOps only operate over individual databases.
deleteWorld :: Database -> IO ()
deleteWorld dbs = do
    (delAccs, delAssets, delContracts) <- mkDeleteBatchOps dbs
    write' (accountDB dbs)  delAccs
    write' (assetDB dbs)    delAssets
    write' (contractDB dbs) delContracts
  where
    write' db toDel =
      LevelDB.write (unDB db) LevelDB.defaultWriteOptions (unTaggedBatchOps toDel)

mkDeleteBatchOps
  :: Database
  -> IO (TaggedBatchOps Account, TaggedBatchOps Asset, TaggedBatchOps Contract)
mkDeleteBatchOps dbs = do
    -- Read current world
    eWorld <- readWorld dbs
    case eWorld of
      Left err -> do
        panic (show err)
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

txDir :: FilePath -> FilePath
txDir root = root </> "txs"

invalidTxDir :: FilePath -> FilePath
invalidTxDir root = root </> "invalidTxs"

currentFile :: FilePath
currentFile = "CURRENT"

lockingFile :: FilePath
lockingFile = "LOCK"

-- | Wipes entire existing DB without prompt
-- Warning: Wipes current DB with no prompting and no exception handling
resetDB
  :: Database
  -> IO (Either LevelDBError ())
resetDB db = do
  dbExists <- doesDirectoryExist $ root db
  if dbExists
    then do
      closeDBs db
      deleteDBs_ db
      createDB $ root db
      pure $ Right ()
    else do
      pure $ Left $ DBDoesNotExist $ toS $ root db

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
    destroy' (txDB db)
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
  , doesDirectoryExist (txDir root)
  , doesDirectoryExist (invalidTxDir root)

   -- Check databases
  , doesFileExist (blockDir root     </> currentFile)
  , doesFileExist (contractDir root  </> currentFile)
  , doesFileExist (assetDir root     </> currentFile)
  , doesFileExist (acctDir root      </> currentFile)
  , doesFileExist (txDir root        </> currentFile)
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
