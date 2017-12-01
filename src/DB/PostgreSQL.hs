{-|

PostgreSQL backend for ledger state.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-type-patterns #-}

module DB.PostgreSQL (

  ConnectionPool,

  PostgresT,
  PostgresM,
  runPostgresT,
  SqlError(..),

  setupDB,
  createDB,
  executeSchema,
  connectDB,
  createAndConnectDB,

  createConnPool,
  closeConnPool,

) where

import Protolude hiding (try)

import Control.Monad.Base
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Distributed.Process.Lifted.Class

import Control.Exception.Lifted (try)

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
import DB.PostgreSQL.InvalidTransaction

type ConnectionPool = Pool.Pool Connection

newtype PostgresT m a = PostgresT
  { unPostgresT :: ReaderT ConnectionPool m a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadReader ConnectionPool)

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
  type Conn (PostgresT m) = Connection
  withConn f = do
    pool <- ask
    liftBase $ Pool.withResource pool f

instance (MonadBase IO m) => MonadReadDB (PostgresT m) where

  readAsset addr     = withConn (flip queryAsset addr)
  readAssets         = withConn queryAssets

  readAccount addr   = withConn (flip queryAccount addr)
  readAccounts       = withConn (fmap Right . queryAccounts)

  readContract addr  = withConn (flip queryContract addr)
  readContracts      = withConn queryContracts

  readBlock index    = withConn (flip queryBlock index)
  readLastBlock      = withConn queryLastBlock
  readBlocks         = withConn queryBlocks
  readLastNBlocks n  = withConn (flip queryLastNBlocks n)

  readInvalidTx hash = withConn (flip queryInvalidTxByHash hash)
  readInvalidTxs     = withConn queryInvalidTxs

  readWorld          = withConn readWorld

instance (MonadBase IO m) => MonadWriteDB (PostgresT m) where

  writeAsset asset     = withConn (flip insertAsset asset)
  writeAssets assets   = withConn (flip insertAssets assets)

  writeAccount acc     = withConn (flip insertAccount acc)
  writeAccounts accs   = withConn (flip insertAccounts accs)

  writeContract c      = withConn (flip insertContract c)
  writeContracts cs    = withConn (flip insertContracts cs)

  writeBlock blk       = withConn (flip insertBlock blk)
  writeBlocks blks     = withConn (flip insertBlocks blks)

  writeInvalidTx itx   = withConn (flip insertInvalidTx itx)
  writeInvalidTxs itxs = withConn (flip insertInvalidTxs itxs)

  resetDB              = withConn (fmap (first show) . resetDB)
  syncWorld world      = withConn (fmap (first show) . flip syncWorld world)

--------------------------------------------------------------------------------
-- PostgreSQL DB Initialization & Connection Management
--------------------------------------------------------------------------------

-- | Load the Uplink schema at compile time using TH
uplinkSchema :: IsString a => a
uplinkSchema =  $(embedStringFile "postgres/uplink.sql")

-- | Execute the uplink schema (create the DB) with the user of the connection
setupDB :: ConnectInfo -> IO (Either SqlError ())
setupDB connInfo = try $ void $ do
    -- Create database with connection user with db name supplied in connInfo
    bracket (connect userConnInfo) close $ \userConn ->
      createDB (toS dbName) userConn
    -- Execute DB schema to build the uplink database
    bracket (connect connInfo) close $ \uplinkConn ->
      executeSchema uplinkConn
  where
    dbUser = connectUser connInfo
    dbName = connectDatabase connInfo

    userConnInfo = connInfo
      { connectDatabase = dbUser }

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

connectDB :: ConnectInfo -> IO ConnectionPool
connectDB conninfo =
  Pool.createPool (connect conninfo) close 1 0.5 1

-- | Connects to the supplied DB and executes the uplink schema, returning the
-- connection pool created for furter use.
createAndConnectDB :: ConnectInfo -> IO ConnectionPool
createAndConnectDB connInfo = do
  pool <- connectDB connInfo
  Pool.withResource pool executeSchema
  pure pool

createConnPool :: Connection -> IO ConnectionPool
createConnPool conn =
  Pool.createPool (pure conn) close 1 0.5 1

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
  -> IO (Either SqlError ())
syncWorld conn world =
  withTransaction conn $ do
    eDel <- deleteWorld conn
    case eDel of
      Left err -> pure $ Left err
      Right _  -> writeWorld conn world

-- | Read the world from the database, failing on the first error.
readWorld
  :: Connection
  -> IO (Either Text Ledger.World)
readWorld conn =
  liftA3 Ledger.mkWorld
    <$> queryAssets conn
    <*> fmap Right (queryAccounts conn)
    <*> queryContracts conn

-- | Atomically write the world state to the database
writeWorld
  :: Connection
  -> Ledger.World
  -> IO (Either SqlError ())
writeWorld conn world = try $ do
    insertAssets conn assets
    insertAccounts conn accounts
    insertContracts conn contracts
  where
    assets    = Map.elems $ Ledger.assets world
    accounts  = Map.elems $ Ledger.accounts world
    contracts = Map.elems $ Ledger.contracts world

-- | Atomically delete the world from the database
deleteWorld
  :: Connection
  -> IO (Either SqlError ())
deleteWorld conn = try $ do
  deleteAssets conn
  deleteAccounts conn
  deleteContracts conn

resetDB
  :: Connection
  -> IO (Either SqlError ())
resetDB conn = try $
  withTransaction conn $ do
    deleteAssets conn
    deleteAccounts conn
    deleteContracts conn
    deleteBlocks conn
    deleteInvalidTxs conn
