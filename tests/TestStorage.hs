module TestStorage (
  storageTests,
) where

import Data.Serialize

import Test.Tasty
import qualified Test.Tasty.HUnit as HUnit
import Test.QuickCheck.Monadic

import qualified Time
import qualified Contract
import qualified Storage
import qualified Derivation

import qualified Script.Eval as Eval
import qualified Script.Compile as Compile

import qualified Reference as Ref

storageTests :: TestTree
storageTests = testGroup "Parser Golden tests"
  [ HUnit.testCase "Script Serialisation" $ do
      ts <- Time.now
      let script = Ref.testScript
          store  = Ref.testStorage
          c      = Eval.scriptToContract ts Ref.testAddr script
          c'     = c { Contract.globalStorage = Storage.GlobalStorage store }
          addr   = Derivation.addrContract c'
          enc    = runPut $ Compile.putScript script (Just store) addr
          dec    = Compile.readScript enc
      HUnit.assertEqual "" (Right (script, Just store, addr)) dec
  ]
