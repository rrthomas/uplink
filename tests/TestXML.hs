module TestXML (
  xmlTests,
) where

import Test.Tasty
import Test.Tasty.Golden

import qualified XML
import qualified Reference

-- | Test that a JSON representation of the core data structures kept invariant.
xmlTests :: TestTree
xmlTests = testGroup "XML Serializer Golden tests"
    -- Structures
    [ goldenVsString "XML Block"
      xmlBlock $ do
        block <- Reference.testGenesis
        pure (XML.toXML [block])

    , goldenVsString "XML Transfer Header"
      xmlTransfer $ pure (XML.toXML [Reference.testTransfer])

    , goldenVsString "XML Create Asset Header"
      xmlCreate $ pure (XML.toXML [Reference.testCreateAsset])

    , goldenVsString "XML Create Account Header"
      xmlAccount $ pure (XML.toXML [Reference.testCreateAccount])

    , goldenVsString "XML Create Contract"
      xmlContract $ pure (XML.toXML [Reference.testCreateContract])
    ]
  where
    -- Structures
    xmlBlock    = "tests/golden/xml/block.xml"
    xmlTransfer = "tests/golden/xml/tx_transfer.xml"
    xmlCreate   = "tests/golden/xml/tx_createAsset.xml"
    xmlAccount  = "tests/golden/xml/tx_createAccount.xml"
    xmlContract = "tests/golden/xml/tx_createContract.xml"
