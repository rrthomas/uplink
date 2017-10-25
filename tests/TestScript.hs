{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestScript (
  scriptPropTests,
  scriptGoldenTests,
) where

import Protolude hiding (Type)

import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.QuickCheck

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.ByteString as BS

import qualified Utils
import qualified Ledger
import qualified Key
import qualified Address
import qualified Encoding
import qualified Contract
import qualified Hash (sha256Raw)

import Script
import Storage
import SafeInteger
import qualified Time
import qualified Homomorphic as Homo
import qualified Script.Eval as Eval
import qualified Script.Pretty as Pretty
import qualified Script.Parser as Parser
import qualified Script.Compile as Compile
import qualified Script.Storage as SStorage

import qualified Reference as Ref

-------------------------------------------------------------------------------
-- Generators
-------------------------------------------------------------------------------

instance Arbitrary BS.ByteString where
  arbitrary = encodeUtf8 . T.pack . ("addr"++)
           <$> (arbitrary :: Gen [Char])

instance Arbitrary Address.Address where
  arbitrary = Address.fromRaw . Encoding.b58 . Hash.sha256Raw <$> arbitrary

instance Arbitrary Loc where
  arbitrary = Loc <$> arbitrary <*> arbitrary

instance Arbitrary Name where
  arbitrary = Name . T.pack . ("name"++)
    <$> listOf1 (elements $ ['a'..'z'] ++ ['1'..'9'] ++ ['_','\''])

instance Arbitrary Label where
  arbitrary = Label . T.pack . ("label"++)
    <$> listOf1 (elements $ ['a'..'z'] ++ ['1'..'9'] ++ ['_','\''])

arbBinOpNm :: Gen Name
arbBinOpNm = Name . T.pack <$> oneof (map pure ["+", "-", "*", "&&", "||"])

instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = Located <$> arbitrary <*> arbitrary

arbNonRecExpr = oneof
  [ EVar <$> arbitrary
  , ELit <$> arbitrary
  ]

arbNonRecLExpr = Located <$> arbitrary <*> arbNonRecExpr

arbNonSeqExpr :: Int -> Gen Expr
arbNonSeqExpr n
  | n <= 0 = arbNonRecExpr
  | otherwise = let n' = n `div` 2 in oneof
      [ ERet    <$> arbNonSeqLExpr n'
      , EAssign <$> arbitrary         <*> arbNonSeqLExpr n'
      , ECall   <$> arbitrary         <*> listOf (arbNonSeqLExpr n')
      , EIf     <$> arbNonSeqLExpr n' <*> arbLExpr n' <*> arbLExpr n'
      -- Don't know how to gen test with taking operator precedence into account
      -- , EBinOp  <$> arbBinOpNm        <*> arbNonSeqLExpr n' <*> arbNonRecLExpr
      ]

arbSeqExpr :: Int -> Gen Expr
arbSeqExpr n
  | n <= 0 = arbNonRecExpr
  | otherwise = let n' = n `div` 2 in
      ESeq <$> arbNonSeqLExpr n' <*> arbLExpr n'

arbLExpr :: Int -> Gen LExpr
arbLExpr n = oneof
  [ arbNonSeqLExpr n, arbSeqLExpr n ]

arbNonSeqLExpr :: Int -> Gen LExpr
arbNonSeqLExpr n = Located <$> arbitrary <*> arbNonSeqExpr n

arbSeqLExpr :: Int -> Gen LExpr
arbSeqLExpr n = Located <$> arbitrary <*> arbSeqExpr n

instance Arbitrary Lit where
  arbitrary = oneof
    [ LInt     <$> arbitrary
    , LFloat   <$> arbitrary
    , LBool    <$> arbitrary
    , LAddress <$> arbitrary
    , pure LVoid
    ]

instance Arbitrary Type where
  arbitrary = oneof
    [ pure TInt
    , pure TFloat
    , pure TBool
    , pure TAccount
    , pure TAsset
    , pure TContract
    , pure TVoid
    ]

instance Arbitrary Def where
  arbitrary = oneof
    [ GlobalDef <$> arbitrary <*> arbitrary <*> arbitrary
    , LocalDef  <$> arbitrary <*> arbitrary <*> arbitrary
    ]

instance Arbitrary Arg where
  arbitrary = Arg <$> arbitrary <*> arbitrary

instance Arbitrary Method where
  arbitrary = Method <$> arbitrary <*> arbitrary <*> arbitrary <*> sized arbLExpr

instance Arbitrary GraphLabel where
  arbitrary = Main <$> arbitrary

instance Arbitrary Script where
  arbitrary = Script <$> arbitrary <*> pure [] <*> arbitrary

scriptPropTests :: TestTree
scriptPropTests = localOption (QuickCheckMaxSize 20) $
  testGroup "Parser and Pretty Printer Tests"
    [ testProperty "AST == parse (ppr AST)" $ \script ->
        let pgmText = Pretty.prettyPrint script in
        case Parser.parseScript pgmText of
          Left err -> False
          Right pgm -> pgm == script
    ]

scriptGoldenTests :: TestTree
scriptGoldenTests = testGroup "Parser Golden tests"
    [ goldenVsStringDiff "Parser test for sample.s" differ parserOutFile $ do
        eSigs <- Compile.compileFile wellTypedFile
        case eSigs of
          Left err -> return $ toSL err
          Right (_,ast) -> return $ toSL $ Utils.ppShow ast
    , goldenVsStringDiff "Signatures (typecheck) test for sample.s" differ sigsOutFile $ do
        eSigs <- Compile.compileFile wellTypedFile
        case eSigs of
          Left err -> return $ toSL err
          Right (sigs,_) -> return $ toSL $ Utils.ppShow sigs
    , goldenVsStringDiff "Ill-type program test for sample_errs.s" differ errsOutFile $ do
        eSigs <- Compile.compileFile illTypedFile
        case eSigs of
          Left err -> return $ toSL err
          Right _  -> return "Type checking succeeded... this should not happen!"
    , goldenVsStringDiff "Eval outputs correct deltas" differ evalOutFile $ do
        eSigs <- Compile.compileFile evalFile
        case eSigs of
          Left err -> return $ toSL err
          Right (_,s) -> do
            now <- Time.now
            let storage = SStorage.initStorage s
            let contract = Eval.scriptToContract now Ref.testAddr s
            Right (pub,priv) <- Homo.genRSAKeyPairSafe 2048
            evalCtx <- initTestEvalCtx pub
            let evalState = Eval.initEvalState contract Ledger.genesisWorld
            eRes <- Eval.execEvalM evalCtx evalState $ Eval.eval s "f" []
            case eRes of
              Left err -> return $ show err
              Right res -> return $ toSL $ Utils.ppShow $ Eval.deltas res

    , goldenVsStringDiff "Eval crypto vals outputs correct deltas" differ evalCryptoOutFile $ do
        Right (pub,priv) <- Homo.genRSAKeyPairSafe 2048
        eSigs <- Compile.compileFile evalCryptoFile
        case eSigs of
          Left err -> return $ toSL err
          Right (_,s) -> do
            now <- Time.now
            let contract = Eval.scriptToContract now Ref.testAddr s
            let gstore = Storage.GlobalStorage $ encryptStorage pub $
                  Storage.unGlobalStorage $ Contract.globalStorage contract
            let contract' = contract { Contract.globalStorage = gstore }
            now <- Time.now
            evalCtx <- initTestEvalCtx pub
            let evalState = Eval.initEvalState contract' Ledger.genesisWorld
            eResEvalState <- Eval.execEvalM evalCtx evalState $ Eval.eval s "f" []
            case eResEvalState of
              Left err -> return $ show err
              Right resEvalState -> do
                let dglobalStore = decryptStorage priv pub $
                      Eval.globalStorage resEvalState
                return $ toSL $ Utils.ppShow dglobalStore

    , goldenVsStringDiff "Compile escrow.s" differ escrowOutFile $ do
        eSigs <- Compile.compileFile escrowFile
        case eSigs of
          Left err -> return $ toSL err
          Right (sigs,_) -> return $ toSL $ Utils.ppShow sigs

    , goldenVsStringDiff "Compile notary.s" differ notaryOutFile $ do
        eSigs <- Compile.compileFile notaryFile
        case eSigs of
          Left err -> return $ toSL err
          Right (sigs,_) -> return $ toSL $ Utils.ppShow sigs

    , goldenVsStringDiff "Compile minimal.s" differ minimalOutFile $ do
        eSigs <- Compile.compileFile minimalFile
        case eSigs of
          Left err -> return $ toSL err
          Right (sigs,_) -> return $ toSL $ Utils.ppShow sigs

    , goldenVsStringDiff "Compile graph.s" differ graphOutFile $ do
        eSigs <- Compile.compileFile graphFile
        case eSigs of
          Left err -> return $ toSL err
          Right (sigs,_) -> return $ toSL $ Utils.ppShow sigs
    ]
  where
    differ ref new = ["diff", "-u", ref, new]

    escrowFile     = "contracts/escrow.s"
    notaryFile     = "contracts/notary.s"
    minimalFile    = "contracts/minimal.s"
    graphFile      = "contracts/graph.s"

    wellTypedFile  = "tests/sample.s"
    illTypedFile   = "tests/sample_errs.s"
    evalFile       = "tests/sample_eval.s"
    evalCryptoFile = "tests/sample_eval_crypto.s"

    escrowOutFile  = "tests/golden/typecheck/escrow.out"
    notaryOutFile  = "tests/golden/typecheck/notary.out"
    minimalOutFile = "tests/golden/typecheck/minimal.out"
    graphOutFile   = "tests/golden/typecheck/graph.out"

    parserOutFile     = "tests/golden/typecheck/parser.out"
    sigsOutFile       = "tests/golden/typecheck/signatures.out"
    errsOutFile       = "tests/golden/typecheck/errors.out"
    evalOutFile       = "tests/golden/typecheck/eval.out"
    evalCryptoOutFile = "tests/golden/typecheck/eval_crypto.out"

-- XXX Move to Storage.hs

encryptStorage :: Homo.PubKey -> Storage -> Storage
encryptStorage pubKey = Map.map encrypt'
  where
    encrypt' :: Value -> Value
    encrypt' v = case v of
      VCrypto n ->
        let Homo.CipherText en = Homo._encrypt pubKey (fromSafeInteger n) 7
        in VCrypto $ toSafeInteger' en
      _ -> v

decryptStorage :: Homo.PrvKey -> Homo.PubKey -> Storage -> Storage
decryptStorage privKey pubKey = Map.map decrypt'
  where
    decrypt' :: Value -> Value
    decrypt' v = case v of
      VCrypto en ->
        let dn = Homo.decrypt privKey pubKey (Homo.CipherText $ fromSafeInteger en)
        in VCrypto $ toSafeInteger' dn
      _ -> v


initTestEvalCtx :: Homo.PubKey -> IO Eval.EvalCtx
initTestEvalCtx pub = do
  now <- Time.now
  pure Eval.EvalCtx
    { Eval.currentBlock = 0
    , Eval.currentValidator = Ref.testAddr
    , Eval.currentTransaction = "TESTHASHOMFG"
    , Eval.currentCreated = now
    , Eval.currentTimestamp = now
    , Eval.currentDeployer = Ref.testAddr
    , Eval.currentTxIssuer = Ref.testAddr
    , Eval.currentAddress = Ref.testAddr
    , Eval.currentPrivKey = Ref.testPriv
    , Eval.currentStorageKey = pub
    }
