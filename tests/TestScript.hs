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

import Control.Monad (fail)

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.ByteString as BS

import qualified Utils
import qualified Ledger
import qualified Fixed
import qualified Key
import qualified Address
import qualified Encoding
import qualified Contract
import qualified Hash (sha256Raw)
import qualified Transaction

import Script
import Storage
import SafeInteger
import qualified SafeString as SafeString
import qualified Time
import qualified Homomorphic as Homo
import qualified Script.Eval as Eval
import qualified Script.Pretty as Pretty
import qualified Script.Parser as Parser
import qualified Script.Compile as Compile
import qualified Script.Typecheck as Typecheck
import qualified Script.Init as Init

import qualified Reference as Ref

-------------------------------------------------------------------------------
-- Generators
-------------------------------------------------------------------------------

instance Arbitrary Fixed.FixedN where
  arbitrary = Fixed.mkFixed <$> arbitrary <*> ((10^6 *) <$> arbitrary)

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
    <$> listOf1 (elements $ ['a'..'z'] ++ ['1'..'9'] ++ ['_'])

instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = Located <$> arbitrary <*> arbitrary

-- This is basically liftArbitrary from Arbitrary1
addLoc :: Gen a -> Gen (Located a)
addLoc g = Located <$> arbitrary <*> g

instance Arbitrary BinOp where
  arbitrary = oneof . map pure $
    [ Add
    , Sub
    , Mul
    , Div
    , And
    , Or
    , Equal
    , NEqual
    , LEqual
    , GEqual
    , Lesser
    , Greater
    ]

instance Arbitrary UnOp where
  arbitrary = pure Not

instance Arbitrary Lit where
  -- Missing literals:
  --  + LDateTime: missing instance Arbitrary DateTime (!)
  --  + LTimeDelta: missing instance Arbitrary TimeDelta (!)
  --  + LAccount: not part of concrete syntax
  --  + LAsset: not part of concrete syntax
  --  + LContract: not part of concrete syntax
  --  + LSig: not part of concrete syntax
  --  + LUndefined: not part of the concrete syntax
  arbitrary = oneof
    [  LInt     <$> arbitrary
    , LFloat   <$> arbitrary
    , LFixed   <$> arbitrary
    , LBool    <$> arbitrary
    , LState   <$> arbitrary
    , LAddress <$> arbitrary
    , pure LVoid
    , LConstr <$> arbitrary
    ]

instance Arbitrary Fixed.PrecN where
  arbitrary = elements [minBound.. maxBound]

instance Arbitrary Type where
  arbitrary = oneof
    [ pure TInt
    , pure TFloat
    , pure TBool
    , pure TAccount
    , pure (TAsset TDiscrete)
    , pure (TAsset TBinary)
    , TAsset . TFractional <$> arbitrary
    , pure TContract
    , pure TVoid
    , TEnum <$> arbitrary
    ]

instance Arbitrary Def where
  arbitrary = oneof
    [ GlobalDef <$> arbitrary <*> arbitrary <*> addLoc (sized arbNonSeqExpr)
    , LocalDef  <$> arbitrary <*> arbitrary <*> addLoc (sized arbNonSeqExpr)
    ]

instance Arbitrary Arg where
  arbitrary = Arg <$> arbitrary <*> arbitrary

instance Arbitrary Method where
  arbitrary = Method <$> arbitrary <*> arbitrary <*> arbitrary <*> sized arbLExpr

instance Arbitrary GraphLabel where
  arbitrary = Main <$> arbitrary

instance Arbitrary Transition where
  arbitrary = Arrow <$> arbState <*> arbState
    where
      arbState = oneof [ pure Initial
                       , pure Terminal
                       , Step <$> arbitrary
                       ]

instance Arbitrary EnumDef where
  arbitrary = EnumDef <$> arbitrary <*> listOf1 arbitrary

instance Arbitrary EnumConstr where
  arbitrary
    = EnumConstr
      . SafeString.fromBytes'
      . encodeUtf8
      . T.pack
      . ("constr"++)
      <$> listOf1 (elements $ ['a'..'z'] ++ ['1'..'9'] ++ ['_','\''])

instance Arbitrary Script where
  arbitrary = Script <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Expr where
  arbitrary = sized arbNonSeqExpr

arbNumLogicExpr :: Int -> Gen Expr
arbNumLogicExpr n
  | n <= 0
    = oneof $ [EVar <$> arbitrary] ++
      map (fmap ELit . addLoc)
            [ LInt <$> arbitrary
            , LFloat <$> arbitrary
            , LFixed <$> arbitrary
            , LBool <$> arbitrary
            ]
  | otherwise = let n' = n `div` 2 in oneof
      [ EBinOp <$> arbitrary
               <*> addLoc (arbNumLogicExpr n')
               <*> addLoc (arbNumLogicExpr n')
      , EUnOp <$> arbitrary <*> addLoc (arbNumLogicExpr n')
      ]

arbMatches :: Int -> Gen [Match]
arbMatches n = listOf1 (Match <$> arbPat <*> arbLExpr n)

arbPat :: Gen LPattern
arbPat = Located <$> arbitrary <*> (PatLit <$> arbitrary)

arbNonSeqExpr :: Int -> Gen Expr
arbNonSeqExpr n
  | n <= 0 = oneof
             [ EVar <$> arbitrary
             , ELit <$> arbitrary
             ]
  | otherwise = let n' = n `div` 2 in oneof
      [ EAssign <$> arbitrary         <*> addLoc (arbNonSeqExpr n')
      , ECall   <$> arbitrary         <*> listOf (addLoc (arbNonSeqExpr n'))
      , EIf     <$> addLoc (arbNonSeqExpr n') <*> arbLExpr n' <*> arbLExpr n'
      , EBefore <$> addLoc (arbNonSeqExpr n') <*> arbLExpr n'
      , EAfter  <$> addLoc (arbNonSeqExpr n') <*> arbLExpr n'
      , EBetween <$> addLoc (arbNonSeqExpr n')
                 <*> addLoc (arbNonSeqExpr n')
                 <*> arbLExpr n'
      , ECase <$> addLoc (arbNonSeqExpr n') <*> arbMatches n'
      , arbNumLogicExpr n
      ]

arbSeqExpr :: Int -> Gen Expr
arbSeqExpr n
  | n <= 0 = arbNonSeqExpr 0
  | otherwise = let n' = n `div` 2 in
      ESeq <$> addLoc (arbNonSeqExpr n') <*> arbLExpr n'

arbLExpr :: Int -> Gen LExpr
arbLExpr n = oneof . map addLoc $
  [ arbNonSeqExpr n, arbSeqExpr n ]

-------------------------------------------------------------------------------
-- Test trees
-------------------------------------------------------------------------------

parserRoundtripTest
  :: (Arbitrary a, Show a, Pretty.Pretty a, Eq a)
  => TestName
  -> (Text -> Either err a)
  -> TestTree
parserRoundtripTest propName parser
  = testProperty propName $ \inp ->
    case parser (Pretty.prettyPrint inp) of
      Left err -> False
      Right outp -> outp == inp

scriptPropTests :: TestTree
scriptPropTests
  = testGroup "Parser and Pretty Printer Tests"
    [ parserRoundtripTest "lit == parse (ppr lit)" Parser.parseLit
    , localOption (QuickCheckMaxSize 20) $
      parserRoundtripTest "expr == parse (ppr expr)" Parser.parseExpr
    , localOption (QuickCheckMaxSize 15) $
      parserRoundtripTest "script == parse (ppr script)" Parser.parseScript
    ]

scriptGoldenTests :: TestTree
scriptGoldenTests = testGroup "Script Compiler Golden Tests"
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
    , evalTest "Eval outputs correct deltas" evalFile evalOutFile "f"
    , evalTest "Eval outputs correct deltas with top-level computation"
               "tests/FCL/sample_eval_toplevel.s"
               "tests/golden/typecheck/eval_toplevel.out"
               "init"
    , goldenVsStringDiff "Eval crypto vals outputs correct deltas" differ evalCryptoOutFile $ do
        Right (pub,priv) <- Homo.genRSAKeyPairSafe 2048
        eBS <- Utils.safeRead evalCryptoFile
        case eBS of
          Left err -> return $ toSL err
          Right bs -> do
            now <- Time.now
            eContract <-
              Init.createFauxContract
                0
                now
                Ref.testAddr
                (Transaction.signature $ Ref.testTx Ref.testCall)
                Ref.testAddr
                Ref.testPriv
                now
                Ref.testAddr
                Ledger.genesisWorld
                (toS bs)
            case eContract of
              Left err -> return $ toSL err
              Right contract -> do
                let gstore = Storage.GlobalStorage $ encryptStorage pub $
                      Storage.unGlobalStorage $ Contract.globalStorage contract
                let contract' = contract { Contract.globalStorage = gstore }
                now <- Time.now
                evalCtx <- initTestEvalCtx pub
                let evalState = Eval.initEvalState contract' Ledger.genesisWorld
                case Contract.lookupContractMethod "f" contract of
                  Left err -> fail $ show err
                  Right method -> do
                    eResEvalState <- Eval.execEvalM evalCtx evalState $ Eval.evalMethod method []
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
    , scriptAnalysisGoldenTests
    , scriptEnumGoldenTests
    ]
  where
    escrowFile     = "contracts/escrow.s"
    notaryFile     = "contracts/notary.s"
    minimalFile    = "contracts/minimal.s"
    graphFile      = "contracts/graph.s"

    wellTypedFile  = "tests/FCL/sample.s"
    illTypedFile   = "tests/FCL/sample_errs.s"
    evalFile       = "tests/FCL/sample_eval.s"
    evalCryptoFile = "tests/FCL/sample_eval_crypto.s"

    escrowOutFile  = "tests/golden/typecheck/escrow.out"
    notaryOutFile  = "tests/golden/typecheck/notary.out"
    minimalOutFile = "tests/golden/typecheck/minimal.out"
    graphOutFile   = "tests/golden/typecheck/graph.out"

    parserOutFile     = "tests/golden/typecheck/parser.out"
    sigsOutFile       = "tests/golden/typecheck/signatures.out"
    errsOutFile       = "tests/golden/typecheck/errors.out"
    evalOutFile       = "tests/golden/typecheck/eval.out"
    evalCryptoOutFile = "tests/golden/typecheck/eval_crypto.out"

differ :: (IsString a) => a -> a -> [a]
differ ref new = ["diff", "-u", ref, new]

evalTest
  :: TestName -- ^ test name
  -> FilePath -- ^ FCL file
  -> FilePath -- ^ expected output
  -> Name -- ^ method name to test (assumed to have no arguments)
  -> TestTree
evalTest testName inputFp outputFp testMethodName =
  goldenVsStringDiff testName differ outputFp $ do
    eBS <- Utils.safeRead inputFp
    case eBS of
      Left err -> return $ toSL err
      Right bs -> do
        now <- Time.now
        Right (pub,priv) <- Homo.genRSAKeyPairSafe 2048
        eContract <-
          Init.createFauxContract
            0
            now
            Ref.testAddr
            (Transaction.signature $ Ref.testTx Ref.testCall)
            Ref.testAddr
            Ref.testPriv
            now
            Ref.testAddr
            Ledger.genesisWorld
            (toS bs)
        case eContract of
          Left err   -> return $ toSL err
          Right contract -> do
            evalCtx <- initTestEvalCtx pub
            let evalState = Eval.initEvalState contract Ledger.genesisWorld
            case Contract.lookupContractMethod testMethodName contract of
              Left err -> fail $ show err
              Right method -> do
                eRes <- Eval.execEvalM evalCtx evalState $ Eval.evalMethod method []
                case eRes of
                  Left err -> return $ show err
                  Right res -> return $ toSL $ Utils.ppShow $ Eval.deltas res

scriptAnalysisGoldenTests :: TestTree
scriptAnalysisGoldenTests = testGroup "Script analysis golden tests"
                            [ graphTests
                            , undefinednessTests
                            , effectTests
                            ]
  where
    graphTests
      = testGroup "Script analysis golden tests"
        [ positiveTest "Reachability test for positive.s"
                       "tests/FCL/graph/positive.s"
                       "tests/golden/graph/positive.out"
        , negativeTest "Invalid transition test for invalid_transition.s"
                       "tests/FCL/graph/invalid_transition.s"
                       "tests/golden/graph/invalid_transition.out"
        , negativeTest "Missing transition test for missing_transition.s"
                       "tests/FCL/graph/missing_transition.s"
                       "tests/golden/graph/missing_transition.out"
        , negativeTest "Reachability check for terminal state for no_path_to_terminal.s"
                       "tests/FCL/graph/no_path_to_terminal.s"
                       "tests/golden/graph/no_path_to_terminal.out"
        , negativeTest "Reachability check for arbitrary nodes for unreachable.s"
                       "tests/FCL/graph/unreachable.s"
                       "tests/golden/graph/unreachable.out"
        , negativeTest "No entry test for no_entry.s"
                       "tests/FCL/graph/no_entry.s"
                       "tests/golden/graph/no_entry.out"
        , negativeTest "No transition into initial state check"
                       "tests/FCL/graph/transition_to_initial.s"
                       "tests/golden/graph/transition_to_initial.out"
        , negativeTest "No transition from terminal state check"
                       "tests/FCL/graph/transition_from_terminal.s"
                       "tests/golden/graph/transition_from_terminal.out"
        , negativeTest "Duplicate transition check"
                       "tests/FCL/graph/duplicate_transition.s"
                       "tests/golden/graph/duplicate_transition.out"
        ]

    undefinednessTests
      = testGroup "Undefinedness checks"
        (map positiveUndefinednessTest
              [ "loop"
              , "if_3"
              , "local"
              ]
        ++ map negativeUndefinednessTest
              [ "different_paths"
              , "global"
              , "if"
              , "if_2"
              , "if_tmp"
              , "states"
              , "assignment"
              , "toplevel"
              ]
         )

    positiveUndefinednessTest testName
      = positiveTest ("Undefinedness check for " <> testName <> ".s")
                     ("tests/FCL/undefinedness/" <> testName <> ".s")
                     ("tests/golden/typecheck/undefinedness/" <> testName <> ".out")

    negativeUndefinednessTest testName
      = negativeTest ("Undefinedness check for " <> testName <> ".s")
                     ("tests/FCL/undefinedness/" <> testName <> ".s")
                     ("tests/golden/typecheck/undefinedness/" <> testName <> ".out")

    effectTests
      = testGroup "Effect analysis golden tests"
        [ negativeTest "Effect analysis for toplevel_side_effect.s"
                       "tests/FCL/effects/toplevel_side_effect.s"
                       "tests/golden/typecheck/effects/toplevel_side_effect.out"
        ]

scriptEnumGoldenTests :: TestTree
scriptEnumGoldenTests = testGroup "Script enum golden tests"
    [ negativeTest "Duplicate constructor test for double_constrs.s"
                   "tests/FCL/enum/double_constrs.s"
                   "tests/golden/typecheck/enum/double_constrs.out"
    , negativeTest "Duplicate enum definition test for double_def.s"
                   "tests/FCL/enum/double_def.s"
                   "tests/golden/typecheck/enum/double_def.out"
    , negativeTest "Empty enum definition test for empty_def.s"
                   "tests/FCL/enum/empty_def.s"
                   "tests/golden/typecheck/enum/empty_def.out"
    , negativeTest "Empty case statement test for empty_match.s"
                   "tests/FCL/enum/empty_match.s"
                   "tests/golden/typecheck/enum/empty_match.out"
    , positiveTest "Type checking test for enum.s"
                   "tests/FCL/enum/enum.s"
                   "tests/golden/typecheck/enum/enum.out"
    , positiveTest "Graph analysis test for graph.s"
                   "tests/FCL/enum/graph.s"
                   "tests/golden/typecheck/enum/graph.out"
    , negativeTest "Incomplete case statement test for incomplete_match.s"
                   "tests/FCL/enum/incomplete_match.s"
                   "tests/golden/typecheck/enum/incomplete_match.out"
    , negativeTest "Overlapping constructors between enums test for overlap_constrs.s"
                   "tests/FCL/enum/overlap_constrs.s"
                   "tests/golden/typecheck/enum/overlap_constrs.out"
    , negativeTest "Overlapping patterns test for overlap_match.s"
                   "tests/FCL/enum/overlap_match.s"
                   "tests/golden/typecheck/enum/overlap_match.out"
    , positiveTest "Undefinedness analysis test for uninitialized.s"
                   "tests/FCL/enum/uninitialized.s"
                   "tests/golden/typecheck/enum/uninitialized.out"
    , negativeTest "Undefined constructor test for unknown_constr.s"
                   "tests/FCL/enum/unknown_constr.s"
                   "tests/golden/typecheck/enum/unknown_constr.out"
    , negativeTest "Undefined constructor test for unknown_enum.s"
                   "tests/FCL/enum/unknown_enum.s"
                   "tests/golden/typecheck/enum/unknown_enum.out"
    ]

positiveTest :: TestName -> FilePath -> FilePath -> TestTree
positiveTest msg inFile outFile
  = goldenVsStringDiff msg differ outFile $ do
    eSigs <- Compile.compileFile inFile
    case eSigs of
      Left err -> panic $ toSL err
      Right (sigs,_) -> return $ toSL $ Utils.ppShow sigs

negativeTest :: TestName -> FilePath -> FilePath -> TestTree
negativeTest msg inFile outFile
  = goldenVsStringDiff msg differ outFile $ do
    eSigs <- Compile.compileFile inFile
    case eSigs of
      Left err -> return $ toSL err
      Right _ -> panic "Script analysis succeeded... this should not happen!"

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
    , Eval.currentTransaction = Transaction.signature $ Ref.testTx Ref.testCall
    , Eval.currentCreated = now
    , Eval.currentTimestamp = now
    , Eval.currentDeployer = Ref.testAddr
    , Eval.currentTxIssuer = Ref.testAddr
    , Eval.currentAddress = Ref.testAddr
    , Eval.currentPrivKey = Ref.testPriv
    , Eval.currentStorageKey = pub
    }
