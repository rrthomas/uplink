{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestStorage (
  storageTests,
) where

import Protolude

import Data.Serialize

import Test.Tasty
import qualified Test.Tasty.HUnit as HUnit
import Test.QuickCheck.Monadic

import qualified Ledger
import qualified Time
import qualified Contract
import qualified Storage
import qualified Transaction

import qualified Script.Eval as Eval
import qualified Script.Compile as Compile
import qualified Script.Init as Init
import qualified Script.Pretty as Pretty

import qualified Reference as Ref

storageTests :: TestTree
storageTests = testGroup "Storage tests"
  [ HUnit.testCase "Script Serialisation" $ do
      ts <- Time.now
      let script = Ref.testScript
          store  = Ref.testStorage
      eContract <-
        Init.createFauxContract
          0
          ts
          Ref.testAddr
          (Transaction.signature $ Ref.testTx Ref.testCall)
          Ref.testAddr
          Ref.testPriv
          ts
          Ref.testAddr
          Ledger.genesisWorld
          (Pretty.prettyPrint script)
      case eContract of
        Left err -> HUnit.assertFailure $ show err
        Right c  -> do
          let c'     = c { Contract.globalStorage = Storage.GlobalStorage store }
              addr   = Contract.address c'
              enc    = runPut $ Compile.putScript script (Just store) addr
              dec    = Compile.readScript enc
          HUnit.assertEqual "" (Right (script, Just store, addr)) dec
  ]
