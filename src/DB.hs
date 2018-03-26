
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DB (
  module DB.Class,
  module DB.Query.Lang,
  module DB.Query.Parser,

  StorageBackend(..),

  initWorld,
  loadWorld,

  initGenesisBlock,
  loadBlocks,

  isTransactionUnique

) where

import Protolude

import DB.Class
import DB.Query.Lang
import DB.Query.Parser

import qualified DB.PostgreSQL
import qualified DB.LevelDB

import Block
import Time
import qualified Account
import qualified Ledger
import qualified Transaction as Tx
import qualified Utils

import qualified Consensus.Authority.Params as CAP

import Database.PostgreSQL.Simple

-- | Storage backend
data StorageBackend
  = LevelDB FilePath
  | PostgreSQL ConnectInfo
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- DB Agnostic State intialization
--------------------------------------------------------------------------------

-- | Load the initial world, backend agnostic
loadWorld
  :: MonadReadDB m
  => (m (Either (DBError m) Ledger.World) -> IO (Either (DBError m) Ledger.World))
  -> IO Ledger.World
loadWorld runDB = do
  eInitWorld <- runDB readWorld
  case eInitWorld of
    Left err -> Utils.dieRed $
      "Could not read world state from Database: " <> show err
    Right initWorld' -> pure initWorld'

-- | Initialize world by loading preallocated accounts and writing them to DB
-- Note: Currently, using PoA, preallocated accounts are validator accounts
initWorld
  :: MonadWriteDB m
  => FilePath
  -> (m (Either (DBError m) ()) -> IO (Either (DBError m) ()))
  -> IO ()
initWorld preallocAccsDir runDB = do
  ePreallocatedAccs <- Account.readAccountsFromDir preallocAccsDir
  case ePreallocatedAccs of
    Left err   -> do
      Utils.putRed $ "Failed to load preallocated accounts from directory: " <> show err
      Utils.dieRed "Please run `uplink keys` to generate validator accounts."
    Right accs -> do
      let eInitWorld = Ledger.addAccounts accs mempty
      case eInitWorld of
        Left err -> Utils.dieRed $
          "Failed to add validator accounts to world state: " <> show err
        Right initWorld -> do
          eRes <- runDB $ syncWorld initWorld
          case eRes of
            Left err -> Utils.dieRed $
              "Failed to write world state to Database: " <> show err
            Right _ -> pure ()

--------------------------------------------------------------------------------

-- | Load a pre-existing genesis block
loadBlocks
  :: MonadReadDB m
  => (m (Either (DBError m) Block) -> IO (Either (DBError m) Block))
  -> IO (Block, Block)
loadBlocks runDB = do
  eGenesisBlock <- runDB $ readBlock 0
  case eGenesisBlock of
    Left err -> do
      Utils.putRed $ "Failed to read genesis block from Database: " <> show err
      Utils.dieRed "Database not intialized."
    Right gBlock -> do
      eLastBlock <- runDB readLastBlock
      case eLastBlock of
        Left err -> do
          Utils.putRed $ "Failed to read last block from Database: " <> show err
          Utils.dieRed "Database not intialized."
        Right lastBlock ->
          pure (gBlock,lastBlock)

-- | Initialize a Genesis Block and write it to the DB
initGenesisBlock
  :: MonadWriteDB m
  => ByteString
  -> Timestamp
  -> CAP.PoA
  -> (m (Either (DBError m) ()) -> IO (Either (DBError m) ()))
  -> IO ()
initGenesisBlock genesisHash genesisTimestamp genPoa runDB = do
  gBlock <- genesisBlock (toS genesisHash) genesisTimestamp genPoa
  eRes <- runDB $ writeBlock gBlock
  case eRes of
    Left err -> Utils.dieRed $
      "Failed to write genesis block to database: " <> show err
    Right _ -> pure ()

--------------------------------------------------------------------------------

-- | Looks up a transaction in the database and returns whether or not the
-- transaction was found or not.
isTransactionUnique :: MonadReadDB m => Tx.Transaction -> m Bool
isTransactionUnique tx =
  either (const True) (const False) <$>
    readTransaction (Tx.base16HashTransaction tx)
