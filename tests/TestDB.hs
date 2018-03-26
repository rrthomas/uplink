{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module TestDB (
  dbTests,
  postgresDatatypeTests,
  storageBackendURITests,
) where

import Protolude hiding (bracket_)

import Test.Tasty
import Test.QuickCheck.Monadic
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HUnit

import Control.Monad.Base
import Control.Exception.Lifted (bracket_)

import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS

import qualified Address
import qualified Asset
import qualified Account
import qualified Contract
import qualified Key
import qualified Block
import qualified Config
import qualified Time
import qualified Storage
import qualified Transaction
import qualified Script.Eval as Eval
import qualified Script.Compile as Compile

import qualified Data.Pool as Pool
import Database.PostgreSQL.Simple
import qualified Database.PostgreSQL.Tmp as PGTmp

import qualified DB
import DB.Class
import DB.LevelDB hiding (close)
import DB.PostgreSQL
import DB.PostgreSQL.Asset
import DB.PostgreSQL.Contract
import DB.PostgreSQL.Block
import DB.PostgreSQL.Error
import DB.Query.Lang

import TestQueryLang (queryLangTests)
import Helpers

import qualified Reference as Ref

dbTests :: TestTree
dbTests =
  testGroup "Testing database backends"
    [ levelDBTests
    , postgresTests
    ]

postgresTests :: TestTree
postgresTests =
     -- Create and use a new conn pool with the DB
     withResource createTmpDB' dropTmpDB' $ \rIO ->
       testGroup "Testing postgres backend" $
         [ dbReadWriteTests "PostgreSQL" $ \action -> do
             (connPool,_,_) <- rIO
             runPostgresT connPool action

         , HUnit.testCase "Resetting DB" $ do
             (connPool,_,_) <- rIO
             eRes :: Either Text () <-
               first show <$> runPostgresT connPool DB.resetDB
             HUnit.assertEqual "Failed to reset test DB" (Right ()) eRes

         , queryLangTests $ \action -> do
             (connPool,_,_) <- rIO
             runPostgresT connPool action
         ]
  where
    testDB = postgreSQLConnectionString testConnInfo
    testConnInfo = defaultConnectInfo
      { connectUser     = "uplink_test_user"
      , connectPassword = "uplink_test_user"
      , connectDatabase = "postgres"
      }

    createTmpDB' = do
      -- create tmp user & db with user_test_user
      (testConn,dbInfo) <-
        PGTmp.createTmpDB testDB
      -- create uplink DB using tmp user
      let tmpConnInfo = testConnInfo
            { connectDatabase = toS $ PGTmp.dbName dbInfo }
      (,testConn,dbInfo) <$> createAndConnectDB' tmpConnInfo

    dropTmpDB' (connPool,testConn,dbInfo) = do
      PGTmp.dropTmpDB (testConn,dbInfo)
      closeConnPool connPool

levelDBTests :: TestTree
levelDBTests =
  withResource (createDB "tmp") deleteDBs $ \dbIO -> do
    dbReadWriteTests "LevelDB" $ \action ->
      flip runLevelDBT action =<< dbIO
  where
    createDB root = do
      eDb <- DB.LevelDB.createDB root
      case eDb of
        Left err -> panic $ show err
        Right db -> pure db

dbReadWriteTests
  :: DB.MonadReadWriteDB m
  => TestName
  -> (m () -> IO ())
  -> TestTree
dbReadWriteTests testNm runDB = do
   testGroup (testNm ++ ": Write/Read all values to/from DB")
     [ HUnit.testCase "Write/Read block tests" testBlocksDB
     , HUnit.testCase "Write/Read assets tests" testAssetsDB
     , HUnit.testCase "Write/Read accounts tests" testAccountsDB
     , HUnit.testCase "Write/Read contracts tests" testContractsDB
     , HUnit.testCase "Write/Read invalidTxs tests" testInvalidTxsDB
     ]
  where
    testBlocksDB = do
      -- Setup
      genesisBlock <- Ref.testGenesis
      block1 <- Ref.testBlock genesisBlock Ref.testTxs
      let blocks = [genesisBlock, block1]
      testDB "writeBlocks -> readBlocks"
        blocks
        DB.writeBlocks
        DB.readBlocks

    testAssetsDB = do
      assets <- Ref.testAssetsDB
      testDB "writeAssets -> readAssets"
        (sortBy (comparing Asset.address) assets)
        DB.writeAssets
        (fmap (sortBy $ comparing Asset.address) <$> DB.readAssets)

    testAccountsDB = do
      let accs = [Ref.testAccount, Ref.testAccount2]
      testDB "writeAccounts -> readAccounts"
        accs
        DB.writeAccounts
        DB.readAccounts

    testContractsDB = do
      let contracts = [Ref.testContract Ref.testTimestamp]
      testDB "writeContracts -> readContracts"
        contracts
        DB.writeContracts
        DB.readContracts

    -- For some reason, LevelDB doesn't read back transactions in the order they
    -- are written to disk... so we need to sort them before and after writing/reading
    sortItxs :: [Transaction.InvalidTransaction] -> [Transaction.InvalidTransaction]
    sortItxs = sortBy (comparing Transaction.base16HashInvalidTx)

    testInvalidTxsDB = do
      testDB "writeInvalidTxs -> readInvalidTxs"
        (sortItxs Ref.testInvalidTxs)
        (DB.writeInvalidTxs)
        (fmap sortItxs <$> DB.readInvalidTxs)

    testDB testName vals write read = do
      runDB $ do
        -- Write/Read
        write vals
        eVals' <- read
        liftBase $ case eVals' of
          Left err -> HUnit.assertFailure (show err)
          Right vals' -> do
            -- Test Equality
            HUnit.assertEqual
              testName
              vals
              vals'

storageBackendURITests :: TestTree
storageBackendURITests =
  testGroup "URI parse tests"
    [ HUnit.testCase "LevelDB URI parsing" $
        uriTest "leveldb:///uplink" (DB.LevelDB "uplink")

    , HUnit.testCase "Postgres URI parsing db" $ do
        uriTest "postgresql://scott:tiger@bootnode:4562/uplink"
          (DB.PostgreSQL (defaultConnectInfo {
            connectUser = "scott"
          , connectPassword = "tiger"
          , connectHost = "bootnode"
          , connectPort = 4562
          , connectDatabase = "uplink"
          }))
        uriTest "postgresql://scott:tiger@bootnode/uplink"
          (DB.PostgreSQL (defaultConnectInfo {
            connectUser = "scott"
          , connectPassword = "tiger"
          , connectHost = "bootnode"
          , connectDatabase = "uplink"
          }))
        uriTest "postgresql://scott@bootnode/uplink"
          (DB.PostgreSQL (defaultConnectInfo {
            connectUser = "scott"
          , connectHost = "bootnode"
          , connectDatabase = "uplink"
          }))
        uriTest "postgresql://scott@bootnode"
          (DB.PostgreSQL (defaultConnectInfo {
            connectUser = "scott"
          , connectHost = "bootnode"
          , connectDatabase = "uplink"
          }))
        uriTest "postgresql://bootnode"
          (DB.PostgreSQL (defaultConnectInfo {
            connectHost = "bootnode"
          , connectDatabase = "uplink"
          }))
    ]
  where
    uriTest uri expected = do
       storageBackend <- Config.getStorageBackend uri
       HUnit.assertEqual uri expected storageBackend

postgresDatatypeTests :: TestTree
postgresDatatypeTests =
  testGroup "PostgreSQL *Row type round trip tests"
    [ HUnit.testCase "Asset <-> AssetRow/HoldingsRows" $ -- Should be tested on an Asset w/ Holdings
        roundTripTest assetToRowTypes (first show . rowTypesToAsset) Ref.testAsset3'
    , HUnit.testCase "Contract <-> ContracRow/GlobalStorageRows/LocalStorageRows" $ do
        contractTs <- Time.now
        let contract = Ref.testContract contractTs
        -- Should be tested on Contracts w/ Global & Local storage
        roundTripTest contractToRowTypes (Right . rowTypesToContract) contract
    , HUnit.testCase "Block <-> BlockRow/TransactionRows" $ do
        genesisBlock <- Ref.testGenesis
        block <- Ref.testBlock genesisBlock Ref.testTxs
        -- Should be tested on a Block with lots of Transactions
        roundTripTest blockToRowTypes (Right . rowTypesToBlock) block
    ]
