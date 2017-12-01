{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestDB (
  dbTests,
  postgresDatatypeTests,
  storageBackendURITests,
) where

import Protolude

import Test.Tasty
import Test.QuickCheck.Monadic
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HUnit

import Control.Monad.Base

import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import qualified Address
import qualified Asset
import qualified DB
import qualified Contract
import qualified Block
import qualified Config
import qualified Time
import qualified Storage
import qualified Derivation
import qualified Script.Eval as Eval
import qualified Script.Compile as Compile

import qualified Data.Pool as Pool
import Database.PostgreSQL.Simple

import DB.Class
import DB.LevelDB hiding (close)
import DB.PostgreSQL
import DB.PostgreSQL.Asset
import DB.PostgreSQL.Contract
import DB.PostgreSQL.Block
import qualified DB.PostgreSQL.Tmp as PGTmp

import qualified Reference as Ref

dbTests :: TestTree
dbTests =
    testGroup "Testing database backends"
      [ runLevelDBTest
 --   , runPostgresTest
      ]
  where
    runPostgresTest :: TestTree
    runPostgresTest =
         -- Create and use a new conn pool with the DB
         withResource createResources destroyResources $ \rIO ->
           dbTestSuite "PostgreSQL" $ \action -> do
             (_,_,connPool) <- rIO
             runPostgresT connPool action
      where
        newDB' = uncurry PGTmp.newDB

        dropRole' :: IO Connection -> Text -> IO ()
        dropRole' connIO txt = void . flip PGTmp.dropRole txt =<< connIO

        dropDB' :: IO Connection -> Text -> IO ()
        dropDB' connIO txt = void . flip PGTmp.dropDB txt =<< connIO

        toConnInfo :: Text -> Text -> ConnectInfo
        toConnInfo dbrole dbname =
          defaultConnectInfo
            { connectUser     = toS dbrole
            , connectDatabase = toS dbname
            }

        createResources :: IO (Text, Text, ConnectionPool)
        createResources = do
          (role,dbname) <-
            bracket PGTmp.defaultDBConn close $ \conn -> do
              role <- PGTmp.newRole conn
              dbname <- PGTmp.newDB conn role
              return (role,dbname)

          let dbConnInfo = toConnInfo role dbname
          pool <- createAndConnectDB dbConnInfo
          return (role,dbname,pool)

        destroyResources :: (Text, Text, ConnectionPool) -> IO ()
        destroyResources (role,dbname,connPool) = do
          closeConnPool connPool
          bracket PGTmp.defaultDBConn close $ \conn ->
            void $ do
              PGTmp.dropDB conn dbname
              PGTmp.dropRole conn role

    runLevelDBTest :: TestTree
    runLevelDBTest =
      withResource (newDB "tmp") deleteDBs $ \dbIO -> do
        dbTestSuite "LevelDB" $ \action ->
          flip runLevelDBT action =<< dbIO

dbTestSuite
  :: DB.MonadReadWriteDB m
  => TestName
  -> (m () -> IO ())
  -> TestTree
dbTestSuite testNm runDB = do
   testGroup (testNm ++ ": Write/Read all values to/from DB")
     [ HUnit.testCase "Write/Read block tests" testBlocksDB
     , HUnit.testCase "Write/Read assets tests" testAssetsDB
     , HUnit.testCase "Write/Read accounts tests" testAccountsDB
     , HUnit.testCase "Write/Read contracts tests" testContractsDB
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
      let assets = Ref.testAssets
      testDB "writeAssets -> readAssets"
        assets
        DB.writeAssets
        DB.readAssets

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

    testDB testName vals write read = do
      runDB $ do
        -- Write/Read
        write vals
        eVals' <- read
        liftBase $ case eVals' of
          Left err -> HUnit.assertFailure (show err)
          Right vals' ->
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
        HUnit.assertBool "Conversion is Isomorphic" $
          roundTripTest assetToRowTypes rowTypesToAsset Ref.testAsset3'
    , HUnit.testCase "Contract <-> ContracRow/GlobalStorageRows/LocalStorageRows" $ do
        contractTs <- Time.now
        let contract = Ref.testContract contractTs
        HUnit.assertBool "Conversion is Isomorphic" $ -- Should be tested on Contracts w/ Global & Local storage
          roundTripTest contractToRowTypes rowTypesToContract contract
    , HUnit.testCase "Block <-> BlockRow/TransactionRows" $ do
        genesisBlock <- Ref.testGenesis
        block <- Ref.testBlock genesisBlock Ref.testTxs
        HUnit.assertBool "Conversion is Isomorphic" $ -- Should be tested on a Block with lots of Transactions
          roundTripTest blockToRowTypes rowTypesToBlock block
    ]

type To a b = a -> b
type From b a = b -> Either Text a

roundTripTest
  :: (Eq a)
  => To a b
  -> From b a
  -> a
  -> Bool
roundTripTest to from x =
  Right x == from (to x)
