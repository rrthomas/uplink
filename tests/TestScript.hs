{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module TestScript (
  scriptCompilerTests,
  scriptPropTests,
) where

import Protolude hiding (Type)

import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Control.Arrow ((&&&))
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
import qualified Script.Prim as Prim

import qualified Reference as Ref
import qualified Hash

-- Note: For some reason, all the eval.out files for golden tests are found in
-- the directory `tests/golden/typecheck` instead of `tests/golden/eval`

scriptCompilerTests :: TestTree
scriptCompilerTests = testGroup "Script Compiler Tests"
  [scriptGoldenTests, ensureExamplesCompileTests]

-------------------------------------------------------------------------------
-- Generators
-------------------------------------------------------------------------------

instance Arbitrary Fixed.FixedN where
  arbitrary = Fixed.mkFixed <$> arbitrary <*> ((10^6 *) <$> arbitrary)

instance Arbitrary BS.ByteString where
  arbitrary = encodeUtf8 . T.pack . ("addr"++)
           <$> (arbitrary :: Gen [Char])

instance Arbitrary (Address.Address a) where
  arbitrary = Address.Address . Hash.sha256 <$> arbitrary

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
  --  + LSig: not part of concrete syntax
  arbitrary = oneof
    [ LInt      <$> arbitrary
    , LFloat    <$> arbitrary
    , LFixed    <$> arbitrary
    , LBool     <$> arbitrary
    , LState    <$> arbitrary
    , LAccount  <$> arbitrary
    , LAsset    <$> arbitrary
    , LContract <$> arbitrary
    , LConstr   <$> arbitrary
    , pure LVoid
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
    [ GlobalDef <$> arbitrary <*> arbitrary <*> arbitrary <*> addLoc (sized arbNonSeqExpr)
    , LocalDef  <$> arbitrary <*> arbitrary <*> addLoc (sized arbNonSeqExpr)
    ]

instance Arbitrary Arg where
  arbitrary = Arg <$> arbitrary <*> arbitrary

instance Arbitrary AccessRestriction where
  arbitrary = oneof
    [ pure RoleAny
    , RoleAnyOf . getNonEmpty <$> arbitrary
    ]

instance Arbitrary Method where
  arbitrary = Method <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> sized arbLExpr

instance Arbitrary Helper where
  arbitrary = Helper <$> arbitrary <*> arbitrary <*> arbitrary

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
  arbitrary = Script <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

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

instance Arbitrary Prim.PrimOp where
  arbitrary = oneof
    [ pure Prim.Verify
    , pure Prim.Sign
    , pure Prim.Block
    , pure Prim.Deployer
    , pure Prim.Sender
    , pure Prim.Created
    , pure Prim.Address
    , pure Prim.Validator
    , pure Prim.Sha256
    , pure Prim.AccountExists
    , pure Prim.AssetExists
    , pure Prim.ContractExists
    , pure Prim.Terminate
    , pure Prim.Now
    , pure Prim.Transition
    , pure Prim.CurrentState
    , pure Prim.TxHash
    , pure Prim.Bound
    , pure Prim.ContractValueExists
    , pure Prim.ContractState
    , pure Prim.NovationInit
    , pure Prim.NovationStop
    , pure Prim.IsBusinessDayUK
    , pure Prim.NextBusinessDayUK
    , pure Prim.IsBusinessDayNYSE
    , pure Prim.NextBusinessDayNYSE
    , pure Prim.Between
    , pure Prim.Fixed1ToFloat
    , pure Prim.Fixed2ToFloat
    , pure Prim.Fixed3ToFloat
    , pure Prim.Fixed4ToFloat
    , pure Prim.Fixed5ToFloat
    , pure Prim.Fixed6ToFloat
    , pure Prim.FloatToFixed1
    , pure Prim.FloatToFixed2
    , pure Prim.FloatToFixed3
    , pure Prim.FloatToFixed4
    , pure Prim.FloatToFixed5
    , pure Prim.FloatToFixed6
    , pure Prim.ContractValue
    , Prim.AssetPrimOp <$> arbAssetPrim
    , Prim.MapPrimOp <$> arbMapPrim
    ]
    where
      arbAssetPrim = elements
        [ Prim.HolderBalance
        , Prim.TransferTo
        , Prim.TransferFrom
        , Prim.CirculateSupply
        , Prim.TransferHoldings
        ]

      arbMapPrim = elements
        [ Prim.MapInsert
        , Prim.MapDelete
        , Prim.MapLookup
        ]

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
  :: (Arbitrary a, Show a, Show err, Pretty.Pretty a, Eq a)
  => TestName
  -> (Text -> Either err a)
  -> TestTree
parserRoundtripTest propName parser
  = testProperty propName $ \inp ->
    case parser (Pretty.prettyPrint inp) of
      Left err   -> False
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
    , evalTest "Eval outputs correct deltas and storage" evalFile evalOutFile [("f", [])] LogDeltas
    , evalTest "Eval outputs correct deltas and storage with top-level computation"
               "tests/FCL/sample_eval_toplevel.s"
               "tests/golden/typecheck/eval_toplevel.out"
               [("init", [])] LogDeltas
    , scriptAnalysisGoldenTests
    , scriptEnumGoldenTests
    , scriptMapGoldenTests
    , scriptSetGoldenTests
    , scriptCollectionGoldenTests
    , scriptHelperGoldenTests
    , scriptDuplicateTests
    ]
  where
    wellTypedFile  = "tests/FCL/sample.s"
    illTypedFile   = "tests/FCL/sample_errs.s"
    evalFile       = "tests/FCL/sample_eval.s"

    notaryOutFile  = "tests/golden/typecheck/notary.out"
    minimalOutFile = "tests/golden/typecheck/minimal.out"
    graphOutFile   = "tests/golden/typecheck/graph.out"

    parserOutFile     = "tests/golden/typecheck/parser.out"
    sigsOutFile       = "tests/golden/typecheck/signatures.out"
    errsOutFile       = "tests/golden/typecheck/errors.out"
    evalOutFile       = "tests/golden/typecheck/eval.out"

differ :: (IsString a) => a -> a -> [a]
differ ref new = ["diff", "-u", ref, new]

data LogDeltas = LogDeltas | NoDeltas

-- | Given a Contract and a list of method names and arguments to each method,
-- accumulate the evalState while evaluation each method in order. Compares the
-- resulting contract storage output with the specified golden file, and
-- potentially the list of deltas.
--
-- The reason we don't want to always log deltas is because homomorphic
-- operations over local variables will result in different intermediate values
-- because different random numbers are used during encryption.
evalTest
  :: TestName          -- ^ test name
  -> FilePath          -- ^ FCL file
  -> FilePath          -- ^ expected output
  -> [(Name, [Value])] -- ^ Sequence of methods to call
  -> LogDeltas         -- ^ Whether or not to log deltas
  -> TestTree
evalTest testName inputFp outputFp funcsAndArgs logDeltas =
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
            (Hash.toHash $ Ref.testTx Ref.testCall)
            Ref.testAddr
            Ref.testPriv
            now
            Ref.testAddr
            Ledger.genesisWorld
            (toS bs)
        case eContract of
          Left err   -> return $ toSL err
          Right contract -> do
            -- Encrypt storage with RSA key
            let helpers = scriptHelpers (Contract.script contract)
                evalState = Eval.initEvalState contract Ledger.genesisWorld
            evalCtx <- initTestEvalCtx helpers
            eResEvalState <- foldM (evalMethod' evalCtx) (Right (contract, evalState)) funcsAndArgs
            case eResEvalState of
              Left err -> return $ show err
              Right (_, resEvalState) -> do
                let globalStoreStr = Utils.ppShow (Eval.globalStorage resEvalState)
                case logDeltas of
                  NoDeltas  -> return (toSL globalStoreStr)
                  LogDeltas -> return $ toSL $ T.intercalate "\n"
                    [ Utils.ppShow (Eval.deltas resEvalState), globalStoreStr ]
  where
    evalMethod' evalCtx accum (name, args) =
      case accum of
        Left err -> fail (show err)
        Right (contract, evalState) ->
          case Contract.lookupContractMethod name contract of
            Left err -> fail (show err ++ "\n" ++ show contract)
            Right method -> do
              eNewEvalState <-
                Eval.execEvalM evalCtx evalState
                  (Eval.evalMethod method args)
              pure ((flip updateContract contract &&& identity) <$> eNewEvalState)

    updateContract evalState contract = contract
      { Contract.globalStorage = Storage.GlobalStorage (Eval.globalStorage evalState)
      , Contract.localStorage  = map Storage.LocalStorage (Eval.localStorage evalState)
      , Contract.state         = Eval.graphState evalState
      }

scriptAnalysisGoldenTests :: TestTree
scriptAnalysisGoldenTests = testGroup "Script analysis golden tests"
                            [ graphTests
                            , undefinednessTests
                            , effectTests
                            , roleSystemTests
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
        , negativeTest "Reachability check 2 for terminal state for no_transition_to_terminal.s"
                       "tests/FCL/graph/no_transition_to_terminal.s"
                       "tests/golden/graph/no_transition_to_terminal.s"
        , negativeTest "Reachability check for arbitrary nodes for unreachable.s"
                       "tests/FCL/graph/unreachable.s"
                       "tests/golden/graph/unreachable.out"
        , negativeTest "No entry test for no_entry.s"
                       "tests/FCL/graph/no_entry.s"
                       "tests/golden/graph/no_entry.out"
        , negativeTest "No transition from terminal state check"
                       "tests/FCL/graph/transition_from_terminal.s"
                       "tests/golden/graph/transition_from_terminal.out"
        , negativeTest "Duplicate transition check"
                       "tests/FCL/graph/duplicate_transition.s"
                       "tests/golden/graph/duplicate_transition.out"
        , negativeTest "No self-loop for terminal state check"
                       "tests/FCL/graph/terminal_self_loop.s"
                       "tests/golden/graph/terminal_self_loop.out"
        , positiveTest "Allow self-loop for initial state check"
                       "tests/FCL/graph/initial_self_loop.s"
                       "tests/golden/graph/initial_self_loop.out"
        , negativeTest "Disallow transition from terminal to initial"
                       "tests/FCL/graph/terminal_to_initial.s"
                       "tests/golden/graph/terminal_to_initial.out"
        ]

    undefinednessTests
      = testGroup "Undefinedness checks"
        (map positiveUndefinednessTest
              [ "loop"
              , "if_3"
              , "local"
              , "access_restriction_ok"
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
              , "access_restriction_bad"
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

    roleSystemTests
      = testGroup "Role system golden tests"
        [ positiveTest "Account literal in method access control (roles_ok.s)"
                       "tests/FCL/roles/roles_ok.s"
                       "tests/golden/roles/roles_ok.out"
        , negativeTest "Duplicate account literal in method access control (roles_duplicate.s)"
                       "tests/FCL/roles/roles_duplicate.s"
                       "tests/golden/roles/roles_duplicate.out"
        , evalTest
            "Eval doesn't proceed when issuer is not authorised" -- negative
            "tests/FCL/roles/roles_ok.s"
            "tests/golden/roles/role_not_authorised.out"
            [("init", [])]
            LogDeltas
        , evalTest
            "Eval proceeds when issuer is authorised" -- positive
            "tests/FCL/roles/roles_test_addr.s"
            "tests/golden/roles/role_authorised.out"
            [("init", [])]
            LogDeltas
        , negativeTest
            "Eval doesn't proceed when issuer is not authorised to edit var"
            "tests/FCL/roles/roles_var_not_authorised.s"
            "tests/golden/roles/role_var_not_authorised.out"
        , positiveTest
            "Eval proceeds when issuer is authorised to edit var"
            "tests/FCL/roles/roles_var_ok.s"
            "tests/golden/roles/role_var_ok.out"
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

scriptMapGoldenTests :: TestTree
scriptMapGoldenTests = testGroup "Script map golden tests"
  [ negativeTest "Invalid types test for invalid_types.s"
                 "tests/FCL/maps/invalid_types.s"
                 "tests/golden/typecheck/maps/invalid_types.out"
  , evalTest "Evaluation of inserts, deletes, and modifies in maps"
             "tests/FCL/maps/eval.s"
             "tests/golden/typecheck/maps/eval.out"
             [("insertInvestor", [VAccount Ref.testAddr,  VEnum (EnumConstr "BigInvestor")])
             ,("insertInvestor", [VAccount Ref.testAddr2, VEnum (EnumConstr "SmallInvestor")])
             ,("insertInvestor", [VAccount Ref.testAddr3, VEnum (EnumConstr "BigInvestor")])
             ,("deleteInvestor", [VAccount Ref.testAddr3])
             ,("lookupInvestor", [VAccount Ref.testAddr])
             ,("rem100Shares",   [VAccount Ref.testAddr])
             ] LogDeltas
  ]

scriptSetGoldenTests :: TestTree
scriptSetGoldenTests = testGroup "Script set golden tests"
  [ negativeTest "Invalid types test for invalid_types.s"
                 "tests/FCL/sets/invalid_types.s"
                 "tests/golden/typecheck/sets/invalid_types.out"
  , evalTest "Evaluation of inserts, and deletes in sets"
             "tests/FCL/sets/eval.s"
             "tests/golden/typecheck/sets/eval.out"
             [("insertInvestor", [VAccount Ref.testAddr,  VEnum (EnumConstr "BigInvestor")])
             ,("insertInvestor", [VAccount Ref.testAddr2, VEnum (EnumConstr "SmallInvestor")])
             ,("insertInvestor", [VAccount Ref.testAddr3, VEnum (EnumConstr "MedInvestor")])
             ,("deleteInvestor", [VAccount Ref.testAddr2, VEnum (EnumConstr "SmallInvestor")])
             ,("deleteInvestor", [VAccount Ref.testAddr,  VEnum (EnumConstr "BigInvestor")])
             ,("end", [])
             ] LogDeltas
  ]

scriptCollectionGoldenTests :: TestTree
scriptCollectionGoldenTests =
  testGroup "Script Collection type golden tests"
    [ negativeTest "Test expected typechecker failure for 'aggregate' primop using 'invalid_aggregate.s'"
                   "tests/FCL/collections/invalid_aggregate.s"
                   "tests/golden/typecheck/collections/invalid_aggregate.out"
    , evalTest "Test eval of higher-order collection prim op 'aggregate' using aggregate.s"
               "tests/FCL/collections/aggregate.s"
               "tests/golden/typecheck/collections/aggregate.out"
               [("sumBalances", [])]
               LogDeltas
    , negativeTest "Test expected typechecker failure for 'transform' primop using 'invalid_transform.s'"
                   "tests/FCL/collections/invalid_transform.s"
                   "tests/golden/typecheck/collections/invalid_transform.out"
    , evalTest "Test eval of higher-order collection prim op 'transform' using transform.s"
               "tests/FCL/collections/transform.s"
               "tests/golden/typecheck/collections/transform.out"
               [("applyInterest",[])]
               LogDeltas
    , negativeTest "Test expected typechecker failure for 'filter' primop using 'invalid_filter.s'"
                   "tests/FCL/collections/invalid_filter.s"
                   "tests/golden/typecheck/collections/invalid_filter.out"
    , evalTest "Test eval of higher-order collection prim op 'filter' using filter.s"
               "tests/FCL/collections/filter.s"
               "tests/golden/typecheck/collections/filter.out"
               [("filterTest",[])] LogDeltas
    , negativeTest "Test expected typechecker failure for 'element' primop using 'invalid_element.s'"
                   "tests/FCL/collections/invalid_element.s"
                   "tests/golden/typecheck/collections/invalid_element.out"
    , evalTest "Test eval of higher-order collection prim op 'element' using element.s"
               "tests/FCL/collections/element.s"
               "tests/golden/typecheck/collections/element.out"
               [("elementTest",[VAccount Ref.testAddr, VInt 500])] LogDeltas
    ]

scriptHelperGoldenTests :: TestTree
scriptHelperGoldenTests =
  testGroup "Script helper functions golden tests"
    [ negativeTest "Self recursion should be disallowed with invalid function name"
                   "tests/FCL/helpers/simple_recursion.s"
                   "tests/golden/typecheck/helpers/simple_recursion.out"
    , negativeTest "Mutual recursion should be disallowed with invalid function name"
                   "tests/FCL/helpers/mutual_recursion.s"
                   "tests/golden/typecheck/helpers/mutual_recursion.out"
    , negativeTest "PrimOp calls and global variable assignments disallowed in helper functions"
                   "tests/FCL/helpers/disallow_effects.s"
                   "tests/golden/typecheck/helpers/disallow_effects.out"
    , negativeTest "Helpers with invalid argument and return should not pass the typechecker"
                   "tests/FCL/helpers/invalid_types.s"
                   "tests/golden/typecheck/helpers/invalid_types.s"
    , evalTest "Helper function can use previously defined helper function"
               "tests/FCL/helpers/helper_using_helper.s"
               "tests/golden/typecheck/helpers/helper_using_helper.out"
               [("flipeeFlopee", [VEnum (EnumConstr "Flop")])
               ,("setX", [VInt 42])
               ] LogDeltas
    , evalTest "Example use of helper functions succeeds"
               "tests/FCL/helpers/helpers.s"
               "tests/golden/typecheck/helpers/helpers.out"
               [("f", [ VAccount (Address.fromRaw "43WRxMNcnYgZFcE36iohqrXKQdajUdAxeSn9mzE1ZedB")
                      , VFixed (Fixed.mkFixed Fixed.Prec2 5000)
                      ]
                )
               ] LogDeltas
    ]


-- TODO move all duplicate tests here
scriptDuplicateTests :: TestTree
scriptDuplicateTests = testGroup "Script duplicate/variable shadowing tests"
  [ negativeTest "Variable shadowing with method argument"
                 "tests/FCL/duplicates/shadowing.s"
                 "tests/golden/typecheck/shadowing.out"
  ]

-- | If compilation succeeds, the test succeeds
positiveTest :: TestName -> FilePath -> FilePath -> TestTree
positiveTest msg inFile outFile
  = goldenVsStringDiff msg differ outFile $ do
    eSigs <- Compile.compileFile inFile
    case eSigs of
      Left err -> panic $ toSL err
      Right (sigs,_) -> return $ toSL $ Utils.ppShow sigs

-- | If compilation fails, the test succeeds
negativeTest :: TestName -> FilePath -> FilePath -> TestTree
negativeTest msg inFile outFile
  = goldenVsStringDiff msg differ outFile $ do
    eSigs <- Compile.compileFile inFile
    case eSigs of
      Left err -> return $ toSL err
      Right _ -> panic "Script analysis succeeded... this should not happen!"

initTestEvalCtx :: [Helper] -> IO Eval.EvalCtx
initTestEvalCtx helpers = do
  now <- Time.now
  pure Eval.EvalCtx
    { Eval.currentBlock = 0
    , Eval.currentValidator = Ref.testAddr
    , Eval.currentTransaction = Hash.toHash $ Ref.testTx Ref.testCall
    , Eval.currentCreated = now
    , Eval.currentTimestamp = now
    , Eval.currentDeployer = Ref.testAddr
    , Eval.currentTxIssuer = Ref.testAddr
    , Eval.currentAddress = Ref.testAddr
    , Eval.currentPrivKey = Ref.testPriv
    , Eval.currentHelpers = helpers
    }

ensureExamplesCompileTests :: TestTree
ensureExamplesCompileTests = withResource
    (findByExtension [".s"] "examples") -- IO [FP]
    (const $ return ())
    (\scripts -> checkExamples scripts)

checkExamples :: IO [FilePath] -> TestTree
checkExamples fs
  = testCase "Compile example files" $ do
      errs <- getErrors =<< fs
      case errs of
        "" -> return ()
        errs -> assertFailure $ T.unpack errs
  where
    getErrors :: [FilePath] -> IO Text
    getErrors = foldM accumulateErrors ""

    accumulateErrors :: Text -> FilePath -> IO Text
    accumulateErrors errs f = Compile.compileFile f >>= \case
      Left msg -> return $ errs <> heading f  <> msg
      Right _ -> return errs

    heading :: FilePath -> Text
    heading f = "\n\n" <> T.pack (f <> "\n" <> replicate 40 '=') <> "\n"
