module TestJson (
  jsonTests,
) where

import Data.Aeson
import Data.Aeson.Encode.Pretty

import Test.Tasty
import Test.Tasty.Golden

import qualified Key
import qualified Block
import qualified Time
import qualified Address
import qualified Account
import qualified Contract
import qualified Storage
import qualified Transaction
import qualified Reference

-- | Test that a JSON representation of the core data structures kept invariant.
jsonTests :: TestTree
jsonTests = testGroup "JSON Serializer Golden tests"
    -- Structures
    [ goldenVsString "JSON Address"
      addrFile
      (pure (encodePretty Reference.testAddr))

    , goldenVsString "JSON Account"
      acctFile
      (pure (encodePretty Reference.testAccount))

    , goldenVsString "JSON Contract"
      contractFile
      (pure (encodePretty (Reference.testContract Reference.testTimestamp)))

    , goldenVsString "JSON Asset"
      assetFile
      (pure (encodePretty (Reference.testAsset1)))

    , goldenVsString "JSON Storage"
      storageFile
      (pure (encodePretty Reference.testStorage))

    , goldenVsString "JSON Block"
      blockFile
      (encodePretty <$> Reference.testGenesis)

    -- Transactions
    , goldenVsString "JSON Transfer Tx"
      transferFile
      (pure $ encodePretty $ Reference.testTx Reference.testTransfer)

    , goldenVsString "JSON Bind Tx"
      bindFile
      (pure $ encodePretty $ Reference.testTx Reference.testBind)

    , goldenVsString "JSON CreateAccount Tx"
      createAccountFile
      (pure $ encodePretty $ Reference.testTx Reference.testCreateAccount)

    , goldenVsString "JSON Revoke Tx"
      revokeFile
      (pure $ encodePretty $ Reference.testTx Reference.testRevokeAccount)

    , goldenVsString "JSON Call Tx"
      callFile
      (pure $ encodePretty $ Reference.testTx Reference.testCall)

    , goldenVsString "JSON SyncLocal Tx"
      syncInitialCommitFile
      (pure $ encodePretty $ Reference.testTx Reference.testSyncLocal)
    ]
  where
    -- Structures
    addrFile              = "tests/golden/json/address.json"
    acctFile              = "tests/golden/json/account.json"
    contractFile          = "tests/golden/json/contract.json"
    storageFile           = "tests/golden/json/storage.json"
    blockFile             = "tests/golden/json/block.json"
    assetFile             = "tests/golden/json/asset.json"

    -- Transactions
    bindFile              = "tests/golden/json/bind.json"
    transferFile          = "tests/golden/json/transfer.json"
    createAccountFile     = "tests/golden/json/createAccount.json"
    revokeFile            = "tests/golden/json/revoke.json"
    callFile              = "tests/golden/json/call.json"
    syncInitialCommitFile = "tests/golden/json/sync_initial_commit.json"
