{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}

module TestQueryLang (
  queryLangTests,
) where

import Protolude hiding (bracket_)

import Test.Tasty
import Test.QuickCheck (Arbitrary(..))
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HUnit

import Control.Monad.Base (liftBase)
import Control.Exception.Lifted (bracket_)

import qualified DB
import DB.PostgreSQL
import DB.PostgreSQL.Asset
import DB.PostgreSQL.Contract
import DB.PostgreSQL.Block
import DB.PostgreSQL.Error
import DB.Query.Lang

import qualified Data.Text as T
import qualified Data.Map as Map

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField

import qualified Account
import qualified Asset
import qualified Contract
import qualified SafeString
import qualified Key
import qualified Reference as Ref
import qualified Script.Graph as Graph
import qualified Script.Pretty as Pretty
import qualified Storage

import KeyTests   -- For Arbitrary PubKey
import TestScript -- For Arbitrary Address

import System.IO.Unsafe

--------------------------------------------------------------------------------
-- Arbitrary Instances for Query Lang Datatypes
--------------------------------------------------------------------------------

genPosInt :: QC.Gen Int
genPosInt = arbitrary `QC.suchThat` (>0)

-- | This sucks
instance Arbitrary Text where
  arbitrary = (psToText <$> arbitrary) `QC.suchThat` (not . T.any (== '\"'))
    where
      psToText (QC.PrintableString s) = T.pack s

instance Arbitrary Table where
  arbitrary = QC.elements
    [ TableAccounts      AccountsTable
    , TableAssets        AssetsTable
    , TableHoldings      HoldingsTable
    , TableContracts     ContractsTable
    , TableGlobalStorage GlobalStorageTable
    , TableLocalStorage  LocalStorageTable
    , TableBlocks        BlocksTable
    , TableTransactions  TransactionsTable
    ]

--------------------------------------------------------------------------------

instance Arbitrary AccountCol where
  arbitrary = QC.oneof
    [ AccountAddress  <$> arbitrary
    , AccountTimezone <$> QC.elements ["GMT", "EST", "EDT"]
    -- Account public keys can't be written in the query string
    -- , AccountPubKey   <$> arbitrary
    ]

--------------------------------------------------------------------------------

instance Arbitrary Asset.Ref where
  arbitrary = QC.elements [minBound.. maxBound]

instance Arbitrary AssetCol where
  arbitrary = QC.oneof
    [ AssetName      <$> arbitrary
    , AssetIssuer    <$> arbitrary
    , AssetIssuedOn  <$> fmap fromIntegral genPosInt
    , AssetReference <$> arbitrary
    , AssetSupply    <$> fmap fromIntegral genPosInt
    , AssetAddress   <$> arbitrary
    , HoldingsCol    <$> fmap HoldingsHolder arbitrary
    ]

instance Arbitrary HoldingsCol where
  arbitrary = QC.oneof
    [ HoldingsAsset   <$> arbitrary
    , HoldingsHolder  <$> arbitrary
    , HoldingsBalance <$> arbitrary
    ]

--------------------------------------------------------------------------------

instance Arbitrary Graph.GraphState where
  arbitrary = QC.oneof
    [ pure Graph.GraphInitial
    , Graph.GraphLabel . Graph.Label <$> arbitrary
    , pure Graph.GraphTerminal
    ]

instance Arbitrary ContractCol where
  arbitrary = QC.oneof
    [ ContractTimestamp <$> fmap fromIntegral genPosInt
    , ContractState     <$> arbitrary
    , ContractOwner     <$> arbitrary
    , ContractAddress   <$> arbitrary
    ]

instance Arbitrary GlobalStorageCol where
  arbitrary = QC.oneof
    [ GSContract          <$> arbitrary
    , GSKey . Storage.Key <$> arbitrary
    ]

instance Arbitrary LocalStorageCol where
  arbitrary = QC.oneof
    [ LSContract          <$> arbitrary
    , LSAccount           <$> arbitrary
    , LSKey . Storage.Key <$> arbitrary
    ]
--------------------------------------------------------------------------------

instance Arbitrary BlockCol where
  arbitrary = QC.oneof
    [ BlockIdx       <$> fmap fromIntegral genPosInt
    , BlockOrigin    <$> arbitrary
    , BlockTimestamp <$> fmap fromIntegral genPosInt
    ]

genTxType = QC.elements
  [ "CreateAccount"
  , "RevokeAccount"
  , "CreateAsset"
  , "RevokeAsset"
  , "Transfer"
  , "Circulate"
  , "Bind"
  , "SyncLocal"
  , "CreateContract"
  , "Call"
  ]

instance Arbitrary TransactionCol where
  arbitrary = QC.oneof
    [ TxOrigin    <$> arbitrary
    , TxType      <$> genTxType
    ]

--------------------------------------------------------------------------------

instance (HasColName a, Arbitrary a, ToField a) => Arbitrary (Cond a) where
  arbitrary = QC.oneof
    [ ColEquals <$> arbitrary
    , ColGT     <$> arbitrary
    , ColGTE    <$> arbitrary
    , ColLT     <$> arbitrary
    , ColLTE    <$> arbitrary
    ]

instance Arbitrary Conj where
  arbitrary = QC.elements [And, Or]

instance (HasColName a, ToField a, Arbitrary a) => Arbitrary (WhereClause a) where
  arbitrary = QC.oneof
    [ Where <$> arbitrary
    , WhereConj <$> arbitrary <*> arbitrary <*> arbitrary
    ]

-- Need Undecideable Instances for this :'(
instance (SelectConstraints a, ToField (ColType a), Arbitrary (ColType a), Arbitrary a) => Arbitrary (Select' a) where
  arbitrary = Select' <$> arbitrary <*> arbitrary

instance Arbitrary Select where
  arbitrary = QC.oneof
    [ SelectAccounts . Select' AccountsTable         <$> arbitrary
    , SelectAssets . Select' AssetsTable             <$> arbitrary
    , SelectContracts . Select' ContractsTable       <$> arbitrary
    , SelectBlocks . Select' BlocksTable             <$> arbitrary
    , SelectTransactions . Select' TransactionsTable <$> arbitrary
    ]

--------------------------------------------------------------------------------
-- Generate Random Columns from existing Ledger Values
--------------------------------------------------------------------------------

accountToCols :: Account.Account -> [AccountCol]
accountToCols Account.Account{..} =
  [ AccountAddress address
  , AccountTimezone $ toS $ SafeString.toBytes timezone
  ]

assetToCols :: Asset.Asset -> [AssetCol]
assetToCols Asset.Asset{..} =
    [ AssetName (toS name)
    , AssetIssuer issuer
    , AssetIssuedOn issuedOn
    , AssetSupply supply
    , AssetType assetType
    , AssetAddress address
    ] ++ assetRef
      ++ map HoldingsCol -- Can't query for holder = <addr1> AND holder = <addr2>
          (take 1 $ holdingsToCols address holdings)
  where
    assetRef =
      case reference of
        Nothing  -> []
        Just ref -> [AssetReference ref]

holdingsToCols address holdings =
  HoldingsAsset address :
    concatMap (uncurry mkHoldingsCol)
      (Map.toList $ Asset.unHoldings holdings)
  where
    mkHoldingsCol holder balance =
      [HoldingsHolder holder, HoldingsBalance balance]

contractToCols :: Contract.Contract -> [ContractCol]
contractToCols Contract.Contract{..} =
  [ ContractTimestamp timestamp
  , ContractState state
  , ContractOwner owner
  , ContractAddress address
  ]

genRandomCols :: [a] -> QC.Gen [a]
genRandomCols = join . fmap QC.sublistOf . QC.shuffle

accountColGen :: Account.Account -> QC.Gen [AccountCol]
accountColGen = genRandomCols . accountToCols

assetColGen :: Asset.Asset -> QC.Gen [AssetCol]
assetColGen = genRandomCols . assetToCols

contractColGen :: Contract.Contract -> QC.Gen [ContractCol]
contractColGen = genRandomCols . contractToCols

holdingsColGen :: Asset.Asset -> QC.Gen [HoldingsCol]
holdingsColGen Asset.Asset{..} = genRandomCols $ holdingsToCols address holdings

-- blocksColGen :: Block.Block -> QC.Gen [BlockCol]
-- transactionsColGen :: Tx.Transaction -> QC.Gen [TransactionCol]

--------------------------------------------------------------------------------
-- Construct Select values from Ledger Value Columns
--------------------------------------------------------------------------------

-- | Currently only creates ColEqual conditions
colsToWhereClause :: (ToField a, HasColName a) => [a] -> Maybe (WhereClause a)
colsToWhereClause = foldr addColCond Nothing
  where
    addColCond c Nothing   = Just $ Where (ColEquals c)
    addColCond c (Just wc) = Just $ WhereConj (ColEquals c) And wc

accountSelectGen :: Account.Account -> QC.Gen (Select' AccountsTable)
accountSelectGen = fmap (Select' AccountsTable . colsToWhereClause) . accountColGen

assetSelectGen :: Asset.Asset -> QC.Gen (Select' AssetsTable)
assetSelectGen = fmap (Select' AssetsTable . colsToWhereClause) . assetColGen

holdingsSelectGen :: Asset.Asset -> QC.Gen (Select' HoldingsTable)
holdingsSelectGen = fmap (Select' HoldingsTable . colsToWhereClause) . holdingsColGen

contractSelectGen :: Contract.Contract -> QC.Gen (Select' ContractsTable)
contractSelectGen = fmap (Select' ContractsTable . colsToWhereClause) . contractColGen

--------------------------------------------------------------------------------

queryLangTests
  :: (PostgresM () -> IO ())
  -> TestTree
queryLangTests runDB =
    testGroup "Query subset tests"
      [ localOption (QC.QuickCheckTests 10000) $
          QC.testProperty "Test Select == parseQuery(prettyPrint(Select))" $ \select ->
            let ppr = Pretty.prettyPrint select
                select' = DB.parseQuery ppr
             in Right select == select'
      , localOption (QC.QuickCheckTests 10000) $
          QC.testProperty "Test validity of generated SQL statements" $ \select ->
            QC.monadicIO $ do
              mvar <- QC.run newEmptyMVar
              QC.run $ runDB $ do
                eRes <- runSelectDB select
                liftBase $ case eRes of
                  Left err -> putMVar mvar False
                  Right _  -> putMVar mvar True
              res <- QC.run $ takeMVar mvar
              QC.assert res
        , testGroup "Test correct results returned by queries" $
            [ QC.testProperty "Test Account queries" $
                queryProp
                  testSelectAcc
                  (accountSelectGen account)
                  "Failed to select account"
            , QC.testProperty "Test Asset queries" $
                queryProp
                  testSelectAsset
                  (assetSelectGen asset)
                  "Failed to select asset"
            , QC.testProperty "Test Contract queries" $
                queryProp
                  testSelectContract
                  (contractSelectGen contract)
                  "Failed to select contract"
            ]
      ]
  where
    -- test values
    account  = Ref.testAccount
    asset    = Ref.testAsset3'
    contract = Ref.testContract Ref.testTimestamp

    ----------------------------------------
    -- Query Helpers
    ----------------------------------------

    queryProp
      :: (Show a)
      => (Select' a -> [Char] -> PostgresM ())
      -> QC.Gen (Select' a)
      -> [Char]
      -> QC.Property
    queryProp testSelect genQ errMsg =
      QC.monadicIO $ do
        sel <- QC.run $ QC.generate genQ
        QC.run $ runDB $ testSelect sel errMsg

    -- Insert test value into DB, removing it on exception or completion
    inEnv :: PostgresM a -> PostgresM b -> PostgresM b
    inEnv mkEnv = bracket_ mkEnv DB.resetDB

    testSelectInEnv
      :: (PostgresM () -> PostgresM ())
      -> Select
      -> [Char]
      -> PostgresM ()
    testSelectInEnv setupEnv select errMsg =
      setupEnv $ do
        eRes <- DB.runSelectDB select
        liftBase $ case eRes of
          Left err  ->
           HUnit.assertFailure errMsg
          Right res ->
            HUnit.assertEqual
              errMsg
              1 -- expect 1 result
              (lenSelectResults res)

    -- Account Query Helpers
    ----------------------------------------

    -- Insert test account into DB, perform test, then remove it.
    inAccountEnv :: PostgresM () -> PostgresM ()
    inAccountEnv = inEnv (DB.writeAccount account)

    testSelectAcc :: Select' AccountsTable -> [Char] -> PostgresM ()
    testSelectAcc = testSelectInEnv inAccountEnv . SelectAccounts

    -- Asset Query Helpers
    ----------------------------------------

    -- Insert test asset into DB, perform test, then remove it.
    inAssetEnv :: PostgresM () -> PostgresM ()
    inAssetEnv = inEnv (DB.writeAsset asset)

    testSelectAsset :: Select' AssetsTable -> [Char] -> PostgresM ()
    testSelectAsset = testSelectInEnv inAssetEnv . SelectAssets

      -- Contract Query Helpers
    ----------------------------------------

    -- Insert test contract into DB, perform test, then remove it.
    inContractEnv :: PostgresM () -> PostgresM ()
    inContractEnv = inEnv (DB.writeContract contract)

    testSelectContract :: Select' ContractsTable -> [Char] -> PostgresM ()
    testSelectContract = testSelectInEnv inContractEnv . SelectContracts
