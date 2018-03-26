module TestJson (
  jsonTests,
) where

import Protolude

import Data.Aeson as A
import Data.Aeson.Encode.Pretty

import qualified Data.ByteString.Lazy as BSL

import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit

import Helpers

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
jsonTests =
  testGroup "JSON Serialize Tests"
    [ testGroup "JSON Serializer Golden tests"
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
    , testGroup "JSON Serializer Round-trip tests"
        [ testCase "JSON Address" $
            roundTripTest A.encode eitherDecodeTextErr Reference.testAddr
        , testCase "JSON Account" $
            roundTripTest A.encode eitherDecodeTextErr Reference.testAccount
        , testCase "JSON Contract" $
            roundTripTest A.encode eitherDecodeTextErr $
              Reference.testContract Reference.testTimestamp
        , testCase "JSON Asset" $
            roundTripTest A.encode eitherDecodeTextErr Reference.testAsset3'
        , testCase "JSON Storage" $
            roundTripTest A.encode eitherDecodeTextErr Reference.testStorage
        , testCase "JSON Transfer Tx" $
            roundTripTest A.encode eitherDecodeTextErr $
              Reference.testTx Reference.testTransfer
        , testCase "JSON Circulate Tx" $
            roundTripTest A.encode eitherDecodeTextErr $
              Reference.testTx Reference.testCirculate
        , testCase "JSON CreateAccount Tx" $
            roundTripTest A.encode eitherDecodeTextErr $
              Reference.testTx Reference.testCreateAccount
        , testCase "JSON RevokeAccount Tx" $
            roundTripTest A.encode eitherDecodeTextErr $
              Reference.testTx Reference.testRevokeAccount
        , testCase "JSON CreateAsset Tx" $
            roundTripTest A.encode eitherDecodeTextErr $
              Reference.testTx Reference.testCreateAsset
        , testCase "JSON RevokeAsset Tx" $
            roundTripTest A.encode eitherDecodeTextErr $
              Reference.testTx Reference.testRevokeAsset
        , testCase "JSON CreateContract  Tx" $
            roundTripTest A.encode eitherDecodeTextErr $
              Reference.testTx Reference.testCreateContract
        , testCase "JSON Call Tx" $
            roundTripTest A.encode eitherDecodeTextErr $
              Reference.testTx Reference.testCall
        , testCase "JSON SyncLocal Tx" $
            roundTripTest A.encode eitherDecodeTextErr $
              Reference.testTx Reference.testSyncLocal
        -- test Bind & SyncLocal Txs (currently unused)
        , testCase "JSON Block" $ do
            genesis <- Reference.testGenesis
            testBlock <- Reference.testBlock genesis Reference.testTxs
            roundTripTest A.encode eitherDecodeTextErr testBlock
        ]
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

eitherDecodeTextErr :: FromJSON a => BSL.ByteString -> Either Text a
eitherDecodeTextErr = first toSL . A.eitherDecode
