module TestTx (
  cerealTests,
) where

import Protolude

import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.QuickCheck

import Hexdump
import Data.Serialize

import qualified Reference
import qualified Transaction

-- Serializer tests
cerealTests :: TestTree
cerealTests = testGroup "Tx Serialisation Tests"
    [ goldenVsStringDiff "CreateAccount" differ accOut $ do
        let hdr = Reference.testCreateAccount
        let tx = Reference.testTx hdr
        pure (dumpBytes (encode tx))

    , goldenVsStringDiff "CreateAsset" differ assOut $ do
        let hdr = Reference.testCreateAsset
        let tx = Reference.testTx hdr
        pure (dumpBytes (encode tx))

    , goldenVsStringDiff "CreateContract" differ conOut $ do
        let hdr = Reference.testCreateContract
        let tx = Reference.testTx hdr
        pure (dumpBytes (encode tx))

    , goldenVsStringDiff "ContractCall" differ callOut $ do
        let hdr = Reference.testCall
        let tx = Reference.testTx hdr
        pure (dumpBytes (encode tx))

    , goldenVsStringDiff "Transfer" differ transOut $ do
        let hdr = Reference.testTransfer
        let tx = Reference.testTx hdr
        pure (dumpBytes (encode tx))

    , goldenVsStringDiff "Bind" differ bindOut $ do
        let hdr = Reference.testBind
        let tx = Reference.testTx hdr
        pure (dumpBytes (encode tx))

    , goldenVsStringDiff "SyncLocal" differ initialCommitOut $ do
        let hdr = Reference.testSyncLocal
        let tx = Reference.testTx hdr
        pure (dumpBytes (encode tx))
    ]
  where
    accOut    = "tests/golden/tx/create_account.out"
    assOut    = "tests/golden/tx/create_asset.out"
    conOut    = "tests/golden/tx/create_contract.out"
    callOut   = "tests/golden/tx/contract_call.out"
    transOut  = "tests/golden/tx/asset_transfer.out"
    bindOut   = "tests/golden/tx/bind.out"
    initialCommitOut   = "tests/golden/tx/sync_local_initial_commit.out"

    differ ref new = ["diff", "-u", ref, new]

    dumpBytes :: ByteString -> LByteString
    dumpBytes x = toS (prettyHex x)
