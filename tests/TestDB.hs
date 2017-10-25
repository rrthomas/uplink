{-# LANGUAGE OverloadedStrings #-}
module TestDB (
  dbTests,
) where

import Test.Tasty
import Test.QuickCheck.Monadic
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HUnit

import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS

import qualified Address
import qualified DB
import qualified Contract
import qualified Time
import qualified Storage
import qualified Derivation
import qualified Script.Eval as Eval
import qualified Script.Compile as Compile

import qualified Reference as Ref

dbTests :: IO DB.Databases -> TestTree
dbTests dbs =
  testGroup "DB"
    [
      HUnit.testCase "Load Script && Save and Load Contract" $ do
        (DB.Databases accDB assDB blkDB conDB) <- dbs

        -- load script
        Right (_, ast) <- Compile.compileFile "tests/sample.s"
        ts <- Time.now
        let c = Eval.scriptToContract ts Ref.testAddr ast
        DB.writeContract conDB c

        let addr = Derivation.addrContract c

        -- save contract
        Right c' <- DB.lookupContract conDB addr

        HUnit.assertEqual "1st Test" c c'

        let glob = Just $ Storage.unGlobalStorage $ Contract.globalStorage c'
        let bs = Compile.writeScript (Contract.script c') glob addr
        -- load contract
        let Right (script, store, addr) = Compile.readScript bs
        let c'' = Eval.scriptToContract ts Ref.testAddr script
        let store' = case store of
                      Nothing -> Contract.globalStorage c''
                      Just s  -> Storage.GlobalStorage s
        let c''' = c'' {Contract.globalStorage = store'}
        ts <- Time.now -- scriptToContract adds Time.now as timestamp... of course they will differ!
        let sharedAddr = Derivation.addrContract' ts store'
            thirdContract = c''' { Contract.timestamp = ts, Contract.address = sharedAddr }
            modFirstContract = c { Contract.timestamp = ts, Contract.address = sharedAddr }
        HUnit.assertEqual "2nd Test" thirdContract modFirstContract

        DB.writeContractWithAddr conDB c''' addr

        Right c'''' <- DB.lookupContract conDB addr
        HUnit.assertEqual "3rd Test" c''' c''''
    ]
