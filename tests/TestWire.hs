module TestWire (
  wireTests,
) where

import Protolude

import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.QuickCheck

import Hexdump
import Data.Serialize

import qualified Utils
import qualified Network.P2P.Message as Message

import qualified Reference as Ref

-- Serializer tests
wireTests :: TestTree
wireTests = testGroup "Wire Protocol Tests"
    [ goldenVsStringDiff "Transaction" differ txOut $ do
        tx <- Ref.txMsg
        pure (dumpBytes (Message.encodeMsg tx))

    , goldenVsStringDiff "Ping" differ pingOut $ do
        tx <- Ref.pingMsg
        pure (dumpBytes (Message.encodeMsg tx))

    , goldenVsStringDiff "GetBlockAtIdx" differ getBlockAtIdxOut $ do
        tx <- Ref.getBlockAtIdxMsg
        pure (dumpBytes (Message.encodeMsg tx))

    , goldenVsStringDiff "Block" differ blockOut $ do
        tx <- Ref.blockMsg
        pure (dumpBytes (Message.encodeMsg tx))

    , goldenVsStringDiff "Version" differ versionOut $ do
        tx <- Ref.versionMsg
        pure (dumpBytes (Message.encodeMsg tx))

    ]
  where
    txOut        = "tests/golden/wire/tx_wire.out"
    getBlockAtIdxOut = "tests/golden/wire/getBlockAtIdx_wire.out"
    blockOut     = "tests/golden/wire/block_wire.out"
    pingOut      = "tests/golden/wire/ping_wire.out"
    versionOut   = "tests/golden/wire/version_wire.out"

    differ ref new = ["diff", "-u", ref, new]

    dumpBytes :: ByteString -> LByteString
    dumpBytes x = toS (prettyHex x)
