module Main (
  main,
) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import System.Directory (removeDirectoryRecursive)

import qualified KeyTests

import qualified TestJson
import qualified TestXML
import qualified TestWire
import qualified TestBinary
import qualified TestPaillier
import qualified TestTxAsset
import qualified TestStorage
import qualified TestDB
import qualified TestTx
import qualified TestScript

import qualified DB
import qualified Reference

-------------------------------------------------------------------------------
-- test Suite
-------------------------------------------------------------------------------

suite :: IO DB.Databases -> TestTree
suite dbs = testGroup "Test Suite" [

  -- Cryptography Tests
    KeyTests.keyTests

  -- JSON Serialize Tests
  , TestJson.jsonTests

  -- XML Serialize Tests
  , TestXML.xmlTests

  -- Script Evaluation Tests
  , TestScript.scriptPropTests

  -- Script Parser/Printer Tests
  , TestScript.scriptGoldenTests

  -- Binary Serialization Tests
  , TestBinary.binaryTests

  -- Pallier Encryption Tests
  , TestPaillier.paillierTests

  -- Wire Protocol Tests
  , TestWire.wireTests

  -- Asset Transaction Testes
  , TestTxAsset.txAssetTests dbs

  -- Contract Storage Tests
  , TestStorage.storageTests

  -- Database Tests
  , TestDB.dbTests dbs

  -- Transaction Serialization Tests
  , TestTx.cerealTests
  ]

-------------------------------------------------------------------------------
-- Environment Provisioning
-------------------------------------------------------------------------------

getResources :: IO DB.Databases
getResources = do
  accDB <- DB.AcctDB <$> DB.create (DB.acctDir "/tmp")
  assDB <- DB.AssetDB <$> DB.create (DB.assetDir "/tmp")
  conDB <- DB.ContractDB <$> DB.create (DB.contractDir "/tmp")
  blkDB <- DB.BlockDB <$> DB.create (DB.blockDir "/tmp")
  return $ DB.Databases {
      DB.accountDB  = accDB
    , DB.assetDB    = assDB
    , DB.contractDB = conDB
    , DB.blockDB    = blkDB
    }

-- XXX Delete databases
release :: DB.Databases -> IO ()
release (DB.Databases accDB assDB blkDB conDB) = do
  DB.close $ DB.unAcctDB accDB
  DB.close $ DB.unAssetDB assDB
  DB.close $ DB.unContractDB conDB
  DB.close $ DB.unBlockDB blkDB

  removeDirectoryRecursive $ DB.acctDir "/tmp"
  removeDirectoryRecursive $ DB.assetDir "/tmp"
  removeDirectoryRecursive $ DB.blockDir "/tmp"
  removeDirectoryRecursive $ DB.contractDir "/tmp"

-------------------------------------------------------------------------------
-- Test Runner
-------------------------------------------------------------------------------

main :: IO ()
main =
  defaultMain $
    withResource getResources release suite
