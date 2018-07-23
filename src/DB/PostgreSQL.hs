{-|

PostgreSQL backend for ledger state.

-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-type-patterns #-}

module DB.PostgreSQL (

  ConnectionPool,
  ConnectInfo(..),

  PostgresT,
  PostgresM,
  runPostgresT,
  SqlError(..),

  setupDB,
  createDB,
  executeSchema,

  connect,
  tryConnectDB,
  connectDB,

  createAndConnectDB,
  createAndConnectDB',

  closeConnPool,

) where

import Protolude hiding (try)

import Control.Monad.Base
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Distributed.Process.Lifted.Class

import Data.FileEmbed

import Data.Map as Map
import qualified Data.Pool as Pool

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types

import qualified Ledger

import DB.Class (MonadDB(..), MonadReadDB, MonadWriteDB)
import qualified DB.Class as DBC
import DB.PostgreSQL.Account
import DB.PostgreSQL.Asset
import DB.PostgreSQL.Contract
import DB.PostgreSQL.Block
import DB.PostgreSQL.Transaction
import DB.PostgreSQL.InvalidTransaction
import DB.PostgreSQL.Error

type ConnectionPool = Pool.Pool Connection

newtype PostgresT m a = PostgresT
  { unPostgresT :: ReaderT ConnectionPool m a
  } deriving (Typeable, Functor, Applicative, Monad, MonadIO, MonadTrans, MonadReader ConnectionPool)

runPostgresT :: ConnectionPool -> (PostgresT m a) -> m a
runPostgresT connPool = flip runReaderT connPool . unPostgresT

type PostgresM = PostgresT IO

--------------------------------------------------------------------------------
-- MonadBase/Control/TransControl Boilerplate
--------------------------------------------------------------------------------

instance MonadBase IO m => MonadBase IO (PostgresT m) where
  liftBase = liftBaseDefault

instance MonadTransControl PostgresT where
  type StT PostgresT a = StT (ReaderT ConnectionPool) a
  liftWith = defaultLiftWith PostgresT unPostgresT
  restoreT = defaultRestoreT PostgresT

instance MonadBaseControl IO m => MonadBaseControl IO (PostgresT m) where
  type StM (PostgresT m) a = ComposeSt PostgresT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM

instance MonadProcess m => MonadProcess (PostgresT m) where
  liftP = PostgresT . liftP

instance MonadProcessBase m => MonadProcessBase (PostgresT m) where
  type StMP (PostgresT m) a = ComposeStP PostgresT m a
  liftBaseWithP = defaultLiftBaseWithP
  restoreMP = defaultRestoreMP

--------------------------------------------------------------------------------
-- DB.Class instances
--------------------------------------------------------------------------------

instance MonadBase IO m => MonadDB (PostgresT m) where
  type DBConn (PostgresT m) = Connection
  type DBError (PostgresT m) = PostgreSQLError
  withConn f = do
    pool <- ask
    liftBase $ Pool.withResource pool f

instance (MonadBase IO m) => MonadReadDB (PostgresT m) where

  readAsset addr     = withConn (flip queryAsset addr)
  readAssets         = withConn queryAssets

  readAccount addr   = withConn (flip queryAccount addr)
  readAccounts       = withConn queryAccounts

  readContract addr  = withConn (flip queryContract addr)
  readContracts      = withConn queryContracts

  readBlock index    = withConn (flip queryBlock index)
  readLastBlock      = withConn queryLastBlock
  readBlocks         = withConn queryBlocks
  readLastNBlocks n  = withConn (flip queryLastNBlocks n)

  readTransaction    = withConn . flip queryTransactionByHash

  readInvalidTx      = withConn . flip queryInvalidTxByHash
  readInvalidTxs     = withConn queryInvalidTxs

  readWorld          = withConn readWorld

instance (MonadBase IO m) => MonadWriteDB (PostgresT m) where

  writeAsset asset     = withConn (fmap ignoreModRows . flip insertAsset asset)
  writeAssets assets   = withConn (fmap ignoreModRows . flip insertAssets assets)

  writeAccount acc     = withConn (fmap ignoreModRows . flip insertAccount acc)
  writeAccounts accs   = withConn (fmap ignoreModRows . flip insertAccounts accs)

  writeContract c      = withConn (fmap ignoreModRows . flip insertContract c)
  writeContracts cs    = withConn (fmap ignoreModRows . flip insertContracts cs)

  writeBlock blk       = withConn (fmap ignoreModRows . flip insertBlock blk)
  writeBlocks blks     = withConn (fmap ignoreModRows . flip insertBlocks blks)

  writeInvalidTx itx   = withConn (fmap ignoreModRows . flip insertInvalidTx itx)
  writeInvalidTxs itxs = withConn (fmap ignoreModRows . flip insertInvalidTxs itxs)

  resetDB              = withConn (fmap ignoreModRows . resetDB)
  syncWorld world      = withConn (flip syncWorld world)

ignoreModRows :: Either a b -> Either a ()
ignoreModRows = second (const ())

--------------------------------------------------------------------------------
-- PostgreSQL DB Initialization & Connection Management
--------------------------------------------------------------------------------

-- | Load the Uplink schema at compile time using TH
uplinkSchema :: IsString a => a
uplinkSchema =  $(makeRelativeToProject "postgres/uplink.sql" >>= embedStringFile)

-- | Create an uplink database, execute the uplink schema, and return the
-- connection pool to the uplink database for futher use. The user of
-- ConnectInfo (`connectUser`) role must exist in the machine's Postgres
-- database and have the priveledge of creating a database.
setupDB :: ConnectInfo -> IO (Either PostgreSQLError ConnectionPool)
setupDB connInfo = tryPostgreSQL $ do
    -- Create database with connection user with db name supplied in connInfo
    bracket (connect initConnInfo) close $ \conn ->
      createDB (toS dbName) conn
    -- Execute DB schema to build the uplink database
    uplinkConnPool <- connectDB connInfo
    Pool.withResource uplinkConnPool $ \conn ->
      executeSchema conn
    pure uplinkConnPool
  where
    dbUser = connectUser connInfo
    dbName = connectDatabase connInfo

    initConnInfo = connInfo
      { connectDatabase = "postgres" }

-- | Create the uplink database with supplied name
-- Warning: Throws exception on failure
createDB :: Text -> Connection -> IO Int64
createDB dbName conn =
  execute conn "CREATE DATABASE ?" (Only $ Identifier dbName)

-- | Execute DB schema to build the uplink database
-- Warning: Throws exception on failure
executeSchema :: Connection -> IO Int64
executeSchema conn =
  execute_ conn uplinkSchema

tryConnectDB :: ConnectInfo -> IO (Either PostgreSQLError ConnectionPool)
tryConnectDB connInfo = tryPostgreSQL $ do
  bracket (connect connInfo) close $ \conn -> do
    (_ :: [Only Int]) <- query_ conn "SELECT 1;"
    pure ()
  connectDB connInfo

-- | Connect to a DB returning a ConnectionPool
-- Warning: The connection pool will not report a failed connection until a
-- connection is asked for (`withResource` is called), at which point the
-- exception will be thrown.
connectDB :: ConnectInfo -> IO ConnectionPool
connectDB connInfo = do
  Pool.createPool (connect connInfo) close 1 0.5 1

createAndConnectDB :: ConnectInfo -> IO (Either PostgreSQLError ConnectionPool)
createAndConnectDB = tryPostgreSQL . createAndConnectDB'

-- | Connects to the supplied DB and executes the uplink schema, returning the
-- connection pool created for furter use.
createAndConnectDB' :: ConnectInfo -> IO ConnectionPool
createAndConnectDB' connInfo = do
  pool <- connectDB connInfo
  Pool.withResource pool executeSchema
  pure pool

closeConnPool :: ConnectionPool -> IO ()
closeConnPool = Pool.destroyAllResources

--------------------------------------------------------------------------------
-- Sync/Read/Write World State
--------------------------------------------------------------------------------

-- | Synchronize the DB with the current world (ledger) state
-- Warning: This uses a PostgreSQL transaction. Do not nest.
syncWorld
  :: Connection
  -> Ledger.World
  -> IO (Either PostgreSQLError ())
syncWorld conn world =
  withTransaction conn $ do
    eDel <- ignoreModRows <$> deleteWorld conn
    case eDel of
      Left err -> pure $ Left err
      Right _  -> ignoreModRows <$> writeWorld conn world

-- | Read the world from the database, failing on the first error.
readWorld
  :: Connection
  -> IO (Either PostgreSQLError Ledger.World)
readWorld conn =
  liftA3 Ledger.mkWorld
    <$> queryAssets conn
    <*> queryAccounts conn
    <*> queryContracts conn

-- | Atomically write the world state to the database
writeWorld
  :: Connection
  -> Ledger.World
  -> IO (Either PostgreSQLError ())
writeWorld conn world =
    ignoreModRows <$> do
      insertAssets conn assets
      insertAccounts conn accounts
      insertContracts conn contracts
  where
    assets    = Map.elems $ Ledger.assets world
    accounts  = Map.elems $ Ledger.accounts world
    contracts = Map.elems $ Ledger.contracts world

-- | Delete the world from the database
deleteWorld
  :: Connection
  -> IO (Either PostgreSQLError Int64)
deleteWorld conn = do
  _ <- deleteAssets conn
  _ <- deleteAccounts conn
  deleteContracts conn

resetDB
  :: Connection
  -> IO (Either PostgreSQLError Int64)
resetDB conn =
  withTransaction conn $ do
    _ <- deleteAssets conn
    _ <- deleteAccounts conn
    _ <- deleteContracts conn
    _ <- deleteBlocks conn
    deleteInvalidTxs conn
