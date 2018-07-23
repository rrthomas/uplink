{-|

Typechecker and elaboration for FCL langauge.

-}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Script.Typecheck (
  -- ** Types
  TypeError(..),
  TypeErrInfo(..),

  -- ** Signatures
  Sig(..),

  -- ** Typechecker
  tcLExpr,
  signatures,
  methodSig,
  tcMethodCall,

  -- ** Pretty Printing
  ppSig,
  ppError,
) where

import Protolude hiding (Type, TypeError, Constraint)
import Unsafe (unsafeIndex)
import Fixed
import Script
import Script.Prim
import Script.Pretty hiding ((<>))
import Utils (duplicates, zipWith3M_)

import Control.Monad.State.Strict (modify')

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Serialize (Serialize)
import qualified Data.Map as Map
import qualified Data.Set as Set

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Type signature
data Sig = Sig [Type] Type
  deriving (Eq, Show, Generic, Serialize)

-- | Type error metadata
-- This type uses ByteString for ad-hoc error messages so that we can get a
-- Serialize instance for free.
data TypeErrInfo
  = UnboundVariable Name                -- ^ Unbound variables
  | InvalidDefinition Name Expr Type Type -- ^ Invalid definition
  | InvalidUnOp UnOp Type               -- ^ Invalid unary op
  | InvalidBinOp BinOp Type Type        -- ^ Invalid binary op
  | UndefinedFunction Name              -- ^ Invocation of non-existent non-primop function
  | InvalidAddress Text                 -- ^ Invalid address
  | InvalidArgType Name Type Type       -- ^ Invalid argument to Method call
  | VarNotFunction Name Type         -- ^ Helpers must be of type 'TFun'
  | InvalidLocalVarAssign Text TypeInfo -- ^ Invalid local variable assignment
  | ArityFail Name Int Int              -- ^ Incorrect # args supplied to function
  | UnificationFail TypeInfo TypeInfo   -- ^ Unification fail
  | CaseOnNotEnum TypeInfo              -- ^ Case analysis on non-enum type
  | UnknownConstructor EnumConstr       -- ^ Reference to undefined constructor
  | UnknownEnum Name                    -- ^ Reference to unknown enum type
  | EmptyMatches                        -- ^ Case statement with empty matches
  | PatternMatchError
    { patMatchErrorMissing :: [EnumConstr]
    , patMatchErrorDuplicate :: [EnumConstr]
    }                                   -- ^ Pattern match failures
  | Impossible Text -- ^ Malformed syntax, impossible
  | InvalidAccessRestriction LExpr Type -- ^
  deriving (Eq, Show, Generic, Serialize)

-- | Type error
data TypeError = TypeError
  { errInfo :: TypeErrInfo
  , errLoc  :: Loc
  } deriving (Eq, Show, Generic, Serialize)

instance Ord TypeError where
  compare te1 te2 = compare (errLoc te1) (errLoc te2)

-- | Source of type
--
-- TODO Since there are functions (helpers) defined now, error messages could be
-- improved by attaching location information to arguments of functions. This
-- way, when a type origin is assigned to the type of an argument to a function,
-- the origin will point directly to the column the argument name is introduced,
-- instead of the column that the helper function name occurs in.
data TypeOrigin
  = OutOfThinAir
  | VariableDefn Name     -- ^ Top level definitions
  | InferredFromVar Name  -- ^ Local method variable assignment
  | InferredFromExpr Expr -- ^ Local method variable assignment
  | InferredFromLit Lit   -- ^ Literal types
  | InferredFromAssetType Name Type -- ^ Holdings type inferred from asset type passed to primop
  | InferredFromHelperDef Name -- ^ Inferred from helper function definition
  | InferredFromCollType Name TCollection -- ^ Inferred from collection type in collection primop
  | InferredFromAssignment Name -- ^ Inferred from assignment to existing variable
  | UnaryOperator UnOp    -- ^ From unary operation
  | BinaryOperator BinOp  -- ^ From binary operation
  | IfCondition           -- ^ From if condition
  | DateTimeGuardPred     -- ^ From DateTime guard predicate
  | DateTimeGuardBody     -- ^ From DateTime guard body
  | Assignment            -- ^ From var assignment
  | FunctionArg Int Name  -- ^ Expr passed as function argument + it's position
  | FunctionRet Name      -- ^ Returned from prip op
  | CasePattern Name      -- ^ Enum type of pattern
  | CaseBody Expr         -- ^ Body of case match
  deriving (Eq, Show, Generic, Serialize)

-- | Type error metadata
data TypeInfo = TypeInfo
  { ttype :: Type        -- ^ What type
  , torig :: TypeOrigin  -- ^ Where did it come from
  , tloc  :: Loc         -- ^ Where is it located
  } deriving (Show, Eq, Generic, Serialize)

tIntInfo, tFloatInfo, tContractInfo, tBoolInfo, tAccountInfo, tDatetimeInfo, tDeltaInfo, tMsgInfo :: TypeOrigin -> Loc -> TypeInfo
tIntInfo = TypeInfo TInt
tFloatInfo = TypeInfo TFloat
tContractInfo = TypeInfo TContract
tBoolInfo = TypeInfo TBool
tAccountInfo = TypeInfo TAccount
tDatetimeInfo = TypeInfo TDateTime
tDeltaInfo = TypeInfo TTimeDelta
tMsgInfo = TypeInfo TMsg

tAssetInfo :: TAsset -> TypeOrigin -> Loc -> TypeInfo
tAssetInfo ta = TypeInfo (TAsset ta)

tFixedInfo :: PrecN -> TypeOrigin -> Loc -> TypeInfo
tFixedInfo p = TypeInfo (TFixed p)

tFunInfo :: [Type] -> TVar -> TypeOrigin -> Loc -> TypeInfo
tFunInfo argTypes tv = TypeInfo (TFun argTypes (TVar tv))

throwErrInferM :: TypeErrInfo -> Loc -> InferM TypeInfo
throwErrInferM tErrInfo loc = do
    modify' $ \inferState ->
      inferState { errs = errs inferState ++ [typeError] }
    return $ TypeInfo TError OutOfThinAir loc
  where
    typeError = TypeError tErrInfo loc

-- | Type inference monad state
data InferState = InferState
  { count   :: Int
  , errs    :: [TypeError]
  , constrs :: [Constraint]
  , varEnv  :: TypeEnv
  } deriving (Show)

instance Monoid InferState where
  mempty = InferState 0 mempty mempty mempty
  mappend (InferState c1 e1 cs1 v1) (InferState c2 e2 cs2 v2) =
    InferState (c1 + c2) (e1 <> e2) (cs1 <> cs2) (v1 <> v2)

data TMeta = Local | Global | Temp | FuncArg | HelperFunc
  deriving (Show)

-- | The Typing environment, mapping global/local/temp variables and helper
-- functions to their types.
newtype TypeEnv = TypeEnv (Map Name (TMeta, TypeInfo))
  deriving (Show)

instance Monoid TypeEnv where
  mempty = TypeEnv mempty
  mappend (TypeEnv t1) (TypeEnv t2) =
    TypeEnv (t1 <> t2)

extendEnv :: TypeEnv -> (Name, TMeta, TypeInfo) -> TypeEnv
extendEnv (TypeEnv varEnv) (x, tmeta, typeInfo) =
  TypeEnv (Map.insert x (tmeta, typeInfo) varEnv)

extendEnvM :: (Name, TMeta, TypeInfo) -> InferM ()
extendEnvM v = modify' $ \s ->
  s { varEnv = extendEnv (varEnv s) v }

removeEnv :: TypeEnv -> Name -> TypeEnv
removeEnv (TypeEnv varEnv) x = TypeEnv (Map.delete x varEnv)

removeEnvM :: Name -> InferM ()
removeEnvM nm = modify' $ \s ->
  s { varEnv = removeEnv (varEnv s) nm }

-- Temporarily extend the type env for the duration of the 'InferM' computation
-- supplied.
localEnvM :: [(Name, TMeta, TypeInfo)] -> InferM a -> InferM a
localEnvM vars action = do
  mapM_ extendEnvM vars
  res <- action
  forM_ vars $ \(nm,_,_) ->
    removeEnvM nm
  pure res

type InferM = ReaderT EnumInfo (State InferState)

runInferM
  :: EnumInfo
  -> InferState
  -> InferM a
  -> Either (NonEmpty TypeError) (a, [Constraint])
runInferM enumInfo initInferState inferM =
    case nonEmpty errs of
      Nothing    -> Right (a, constrs)
      Just nerrs -> Left nerrs
  where
    (a, InferState _ errs constrs _) =
      runInferM' enumInfo initInferState inferM

runInferM'
  :: EnumInfo -> InferState -> InferM a -> (a, InferState)
runInferM' enumInfo inferState act
  = runState (runReaderT act enumInfo) inferState

-------------------------------------------------------------------------------
-- Type Signatures for Methods
-------------------------------------------------------------------------------

-- | Typechecks whether the values supplied as arguments to the method
-- call match the method argument types expected
tcMethodCall :: EnumInfo -> Method -> [Value] -> Either TypeErrInfo ()
tcMethodCall enumInfo method argVals
  = do
  actualTypes <- mapM valueType argVals
  zipWithM_ validateTypes expectedTypes actualTypes
  where
    valueType :: Value -> Either TypeErrInfo Type
    valueType val
      = case (val, mapType enumInfo val) of
          (_, Just ty) -> pure ty
          (VEnum c, Nothing) -> Left $ UnknownConstructor c
          (o, Nothing) -> Left $ Impossible $ "Malformed value: " <> show o

    expectedTypes = Script.argtys' method

    validateTypes expected actual =
      when (not $ validMethodArgType expected actual) $
        Left $ InvalidArgType (methodName method) expected actual

    validMethodArgType :: Type -> Type -> Bool
    validMethodArgType (TAsset _) TAssetAny = True
    validMethodArgType TAssetAny (TAsset _) = True
    validMethodArgType t1 t2 = t1 == t2

signatures :: Script -> Either (NonEmpty TypeError) [(Name,Sig)]
signatures (Script enums defns graph methods helpers) =
    case tcErrs of
      Nothing   -> Right methodSigs
      Just errs -> Left (NonEmpty.nub errs)
  where
    enumInfo = createEnumInfo enums
    inferState = snd $
      runInferM' enumInfo mempty $
        tcDefns defns >> tcHelpers helpers

    (tcErrs, methodSigs) = first (fmap sconcat . nonEmpty) $
      partitionEithers $ map (methodSig enumInfo inferState) methods

-- | Typechecks a top-level 'Method' function body and returns a type signature
-- This type 'Sig' differs from helper functions because there is a distinct
-- difference between helper functions and methods. Methods are *not* callable
-- from other methods or functions, whereas helper functions are able to be
-- called from any method or other helper functions.
methodSig
  :: EnumInfo
  -> InferState
  -> Method
  -> Either (NonEmpty TypeError) (Name, Sig)
methodSig enumInfo initInferState m@(Method _ access methNm args body) = do
    checkAccessRestriction access
    -- Typecheck body of method, generating constraints along the way
    let (sig, resInferState) =
          runInferM' enumInfo initInferState $
            functionSig (methNm, args, body)
    case runSolverM (errs resInferState) (constrs resInferState) of
      Left errs   -> Left errs
      Right subst -> fmap (methNm,) $ Right sig
  where
    checkAccessRestriction :: AccessRestriction -> Either (NonEmpty TypeError) ()
    checkAccessRestriction RoleAny = Right () -- nothing to check
    checkAccessRestriction (RoleAnyOf group)
      = case mapMaybe checkAddr group of
          errs@(_:_) -> Left . NonEmpty.fromList $ errs
          [] -> Right ()
      where
        checkAddr :: LExpr -> Maybe TypeError
        checkAddr expr = do
          let (TypeInfo retType _ _, _) =
                runInferM' enumInfo initInferState (tcLExpr expr)
          case retType of
            TAccount -> Nothing
            _ -> Just $ TypeError (InvalidAccessRestriction expr retType) (located expr)

tcHelpers :: [Helper] -> InferM ()
tcHelpers helpers =
  forM_ helpers $ \helper -> do
    helperInfo <- tcHelper helper
    extendEnvM (locVal (helperName helper), HelperFunc, helperInfo)

-- | Typechecks a 'Helper' function body and returns a TypeInfo of TFun,
-- representing the type of a function. This differs from the 'Sig' value
-- returned, because helper functions are injected into the variable environment
-- as variables with 'TFun' types.
tcHelper
  :: Helper
  -> InferM TypeInfo
tcHelper (Helper fnm args body) = do
  (Sig argTypes retType) <- functionSig (locVal fnm,args,body)
  let tfun = TFun argTypes retType
  pure (TypeInfo tfun (InferredFromHelperDef (locVal fnm)) (located fnm))

-- | Gives a function signature to function-esque tuples:
-- Given a triple of a function name, a list of arguments and an expression
-- find the type signature of the expression body (using some initial state)
functionSig
  :: (Name, [Arg], LExpr)
  -> InferM Sig
functionSig (fnm, args, body) = do
  argInfos <- zipWithM (tcArg fnm) args [1..]
  localEnvM argInfos $ do
    retType <- tcLExpr body
    pure $ Sig (map argType args) (ttype retType)

-- | Typechecks the argument and returns the type info of the function argument
-- such that it can be added to the typing env in the manner the caller prefers.
tcArg :: Name -> Arg -> Int -> InferM (Name, TMeta, TypeInfo)
tcArg fnm (Arg typ (Located loc anm)) argPos =
    case typ of
      TEnum enm -> do
        enumDoesExist <- enumExists enm
        if enumDoesExist
           then pure argInfo
           else do
             terrInfo <- throwErrInferM (UnknownEnum enm) loc
             pure (anm, FuncArg, terrInfo)
      _ -> pure argInfo
  where
    argInfo = (anm, FuncArg, TypeInfo typ (FunctionArg argPos fnm) loc)

    enumExists :: Name -> InferM Bool
    enumExists enumNm = do
      enumConstrs <- enumToConstrs <$> ask
      case Map.lookup enumNm enumConstrs of
        Nothing -> pure False
        Just _  -> pure True

-------------------------------------------------------------------------------
-- Typechecker (w/ inference)
-------------------------------------------------------------------------------

tcDefn :: Def -> InferM ()
tcDefn def = extendEnvM =<< case def of
  -- GlobalDef variable can be any type
  GlobalDefNull typ _ lnm -> do
    let Located loc nm = lnm
        typeInfo = TypeInfo typ (VariableDefn nm) loc
    return (nm, Global, typeInfo)

  -- LocalDefNull variable can be any type
  LocalDefNull typ lnm -> do
    let Located loc nm = lnm
        typeInfo = TypeInfo typ (VariableDefn nm) loc
    return (nm, Local, typeInfo)

  GlobalDef typ _ nm lexpr -> do
    exprTypeInfo@(TypeInfo _ _ loc) <- tcLExpr lexpr
    let typeInfo = TypeInfo typ (VariableDefn nm) loc
    case unifyDef nm lexpr typeInfo exprTypeInfo of
      Left terr -> void $ throwErrInferM terr loc
      Right _   -> pure ()
    return (nm, Global, typeInfo)

  -- XXX Local variables are just the same as global vars...
  LocalDef typ nm lexpr -> do
    exprTypeInfo@(TypeInfo _ _ loc) <- tcLExpr lexpr
    let typeInfo = TypeInfo typ (VariableDefn nm) loc
    case unifyDef nm lexpr typeInfo exprTypeInfo of
      Left terr -> void $ throwErrInferM terr loc
      Right _   -> pure ()
    return (nm, Global, typeInfo)
  where
    -- Check if the stated definition type and the rhs expr type match
    unifyDef :: Name -> LExpr -> TypeInfo -> TypeInfo -> Either TypeErrInfo ()
    unifyDef nm le t1 t2 = do
      case runSolverM [] [Constraint (Just le) t1 t2] of
        Left (terr :| _) ->
          case errInfo terr of
            UnificationFail ti1 ti2 -> Left $ InvalidDefinition nm (locVal le) (ttype ti1) (ttype ti2)
            otherwise               -> panic "Solver should fail with UnificationFail"
        Right _                     -> Right ()

tcDefns :: [Def] -> InferM ()
tcDefns = mapM_ tcDefn

tcLExpr :: LExpr -> InferM TypeInfo
tcLExpr le@(Located loc expr) = case expr of
  ENoOp     -> return $ TypeInfo TVoid OutOfThinAir loc
  EVar nm   -> snd <$> lookupVarType' nm
  ELit llit -> tcLLit llit
  ESeq e e' -> tcLExpr e >> tcLExpr e'
  EBinOp nm e1 e2 -> tcBinOp nm e1 e2
  EUnOp nm e -> tcUnOp nm e

  ECall mnm argExprs ->
    case mnm of
      Left primOp    -> tcPrim le loc primOp argExprs
      Right helperNm -> do
        let hnm = locVal helperNm
        mTypeInfo <- lookupVarType helperNm
        case mTypeInfo of
          Nothing -> throwErrInferM (UndefinedFunction hnm) loc
          Just (_, tinfo) ->
            case tinfo of
            -- If a TFun type, generate constraints for all the arguments the
            -- function is applied to, and type the whole expression as the
            -- function's return type.
              TypeInfo (TFun argTypes retType) _ _  -> do
                argTypeInfos <- mapM tcLExpr argExprs
                let mkArgTypeInfo ty n = TypeInfo ty (FunctionArg n hnm) loc
                    argTypeInfos' = zipWith mkArgTypeInfo argTypes [1..]
                zipWith3M_ addConstr argExprs argTypeInfos' argTypeInfos
                pure $ TypeInfo retType (FunctionRet hnm) loc
              -- If error typechecking helper, leave it be
              TypeInfo TError _ _ -> pure tinfo
              -- If any other type, report the error but continue
              TypeInfo varType _ _  -> do
                let terr = VarNotFunction (locVal helperNm) varType
                throwErrInferM terr (located helperNm)

  EAssign nm e -> do
    mVarTypeInfo <- lookupVarType (Located loc nm)
    case mVarTypeInfo of
      Nothing -> do -- New temp variable, instantiate it
        eTypeInfo@(TypeInfo eType _ eLoc)  <- tcLExpr e
        let typeInfo = TypeInfo eType (InferredFromExpr $ locVal e) eLoc
        extendEnvM (nm, Temp, typeInfo)
      Just (varMeta, varTypeInfo) -> do
        tvar <- freshTVar
        let retTypeInfo = TypeInfo tvar (InferredFromAssignment nm) (located e)
        addConstr e varTypeInfo retTypeInfo
        eTypeInfo@(TypeInfo eType _ eLoc)  <- tcLExpr e
        addConstr e retTypeInfo eTypeInfo

    return $ TypeInfo TVoid Assignment (located e)

  EBefore edt e -> do
    dtTypeInfo <- tcLExpr edt
    eTypeInfo  <- tcLExpr e
    addConstr edt (tDatetimeInfo DateTimeGuardPred loc) dtTypeInfo
    addConstr e (TypeInfo TVoid DateTimeGuardBody loc) eTypeInfo
    return $ TypeInfo TVoid DateTimeGuardBody loc

  EAfter edt e -> do
    dtTypeInfo <- tcLExpr edt
    eTypeInfo  <- tcLExpr e
    addConstr edt (tDatetimeInfo DateTimeGuardPred loc) dtTypeInfo
    addConstr e (TypeInfo TVoid DateTimeGuardBody loc) eTypeInfo
    return $ TypeInfo TVoid DateTimeGuardBody loc

  EBetween startDte endDte e -> do
    startTypeInfo <- tcLExpr startDte
    endTypeInfo <- tcLExpr endDte

    let dtInfo = tDatetimeInfo DateTimeGuardPred loc
    addConstr startDte dtInfo startTypeInfo
    addConstr endDte dtInfo endTypeInfo

    eTypeInfo  <- tcLExpr e
    addConstr e (TypeInfo TVoid DateTimeGuardBody loc) eTypeInfo

    return $ TypeInfo TVoid Assignment loc

  EIf cond e1 e2 -> do
    cTypeInfo  <- tcLExpr cond
    e1TypeInfo <- tcLExpr e1
    e2TypeInfo <- tcLExpr e2
    let cTypeInfo' = TypeInfo
          { ttype = TBool
          , torig = IfCondition
          , tloc  = tloc cTypeInfo
          }
    addConstr cond cTypeInfo' cTypeInfo
    addConstr e1 e1TypeInfo e2TypeInfo
    let retTypeInfo = TypeInfo
          { ttype = ttype e1TypeInfo
          , torig = IfCondition
          , tloc  = loc
          }
    return retTypeInfo

  ECase scrut ms -> do
    dTypeInfo <- tcLExpr scrut

    tcCasePatterns scrut dTypeInfo . map matchPat $ ms

    -- Associate every body with its type info
    let attachInfo (Match _ bodyExpr) = (bodyExpr,) <$> tcLExpr bodyExpr
    bodiesTypeInfos <- mapM attachInfo ms
    -- Add a constraint for every body to have the same type as the
    -- first body
    case bodiesTypeInfos of
      [] -> throwErrInferM EmptyMatches $ located scrut
      ((bodyExpr, tyInfo):eis) -> do
        mapM_ (addConstr bodyExpr tyInfo . snd) eis
        return TypeInfo
          { ttype = ttype tyInfo
          , torig = CaseBody (locVal bodyExpr)
          , tloc  = located bodyExpr
          }

tcCasePatterns
  :: LExpr
  -> TypeInfo
  -> [LPattern]
  -> InferM ()
tcCasePatterns scrut scrutInfo []
  = void
    . throwErrInferM EmptyMatches
    $ (located scrut)
tcCasePatterns scrut scrutInfo ps@(_:_)
  = case ttype scrutInfo of
      -- Scrutinee of enum type e
      (TEnum e) -> do
        maybeAllConstrs <- Map.lookup e . enumToConstrs <$> ask
        case maybeAllConstrs of
          Nothing
            -> void $ throwErrInferM (UnknownEnum e) topLoc

          Just allConstrs
            -> case (missing allConstrs ps, overlap ps) of
                 ([],[])
                   -> do
                   enumConstrs <- constrToEnum <$> ask
                   mapM_ (addConstr scrut scrutInfo <=< patternInfo enumConstrs) ps

                 (misses, overlaps)
                   -> void $ throwErrInferM (PatternMatchError misses overlaps) topLoc
      _ -> void $ throwErrInferM (CaseOnNotEnum scrutInfo) topLoc
  where
    topLoc = located scrut

    patConstr (Located _ (PatLit c)) = c
    missing allConstrs ps = allConstrs List.\\ map patConstr ps
    overlap ps = duplicates (map patConstr ps)

    patternInfo enumConstrs (Located ploc (PatLit c))
      = case Map.lookup c enumConstrs of
          Nothing
            -> throwErrInferM (UnknownConstructor c)
                              topLoc
          Just enumName
            -> return TypeInfo
               { ttype = TEnum enumName
               , torig = CasePattern enumName
               , tloc = ploc
               }

tcLLit :: LLit -> InferM TypeInfo
tcLLit (Located loc lit) = do
  enumConstrs <- constrToEnum <$> ask
  case tcLit enumConstrs lit of
    Left err  -> throwErrInferM err loc
    Right typ -> pure $
      TypeInfo typ (InferredFromLit lit) loc

tcLit :: Map EnumConstr Name -> Lit -> Either TypeErrInfo Type
tcLit enumConstrs lit =
  case lit of
    LInt _         -> Right TInt
    LFloat _       -> Right TFloat
    LFixed fixedn  -> Right $ tcFixedN fixedn
    LBool _        -> Right TBool
    LVoid          -> Right TVoid
    LMsg _         -> Right TMsg
    LSig _         -> Right TSig
    LAccount addr  -> Right TAccount
    LAsset addr    -> Right TAssetAny
    LContract addr -> Right TContract
    LState label   -> Right TState
    LDateTime _    -> Right TDateTime
    LTimeDelta _   -> Right TTimeDelta
    LMap lmap      ->
      case Map.toList lmap of
        [] -> Right (TColl (TMap TAny TAny))
        ((k,v):_) -> TColl <$> (TMap <$> tcLit enumConstrs k <*> tcLit enumConstrs v)
    LSet lset ->
      case Set.toList lset of
        [] -> Right (TColl (TSet TAny))
        (v:_) -> TColl <$> (TSet <$> tcLit enumConstrs v)
    LConstr c      ->
      case Map.lookup c enumConstrs of
        Nothing -> Left (Impossible "Reference to unknown enum constructor")
        Just enum -> Right (TEnum enum)

tcFixedN :: FixedN -> Type
tcFixedN = TFixed . \case
  Fixed1 _ -> Prec1
  Fixed2 _ -> Prec2
  Fixed3 _ -> Prec3
  Fixed4 _ -> Prec4
  Fixed5 _ -> Prec5
  Fixed6 _ -> Prec6

-------------------------------------------------------------------------------
  -- Type checking of prim op calls
-------------------------------------------------------------------------------

tcPrim :: LExpr -> Loc -> PrimOp -> [LExpr] -> InferM TypeInfo
tcPrim le eLoc prim argExprs = do

    -- Setup some prim op agnostic varEnvironment
    -- 1) Lookup what the type of the arguments of the prim op should be
    -- 2) Typecheck the arg exprs supplied to the prim op call
    -- 3) Create the type infos of the arg types
    (Sig argTypes retType) <- primSig prim
    arityCheck (Located eLoc primNm) argTypes argExprs
    argExprTypeInfos <- mapM tcLExpr argExprs
    let argTypeOrig n = FunctionArg n primNm
        mkArgTypeInfo t (lexpr, n) = TypeInfo t (argTypeOrig n) $ located lexpr
        argTypeInfos = zipWith mkArgTypeInfo argTypes (zip argExprs [1..])
        retTypeInfo = TypeInfo retType (FunctionRet primNm) eLoc

    -- Ok now typecheck
    case prim of

      -- AssetPrimOps must be typechecked uniquely-- Sometimes the type of one of
      -- the arguments to the prim op is dependent on the type of the asset
      -- supplied, and other times the _return type_ is dependent on the type of
      -- the asset supplied  as an argument.
      AssetPrimOp assetPrimOp ->
        case assetPrimOp of

          HolderBalance    -> do
            let [assetAnyExpr, accExpr]  = argExprs
                [tassetAnyInfo,taccInfo] = argTypeInfos
                [tassetInfo,taccInfo']   = argExprTypeInfos
            -- add constraint for 1st arg to be an asset type
            addConstr assetAnyExpr tassetAnyInfo tassetInfo
            -- add constraint for 2nd arg to be an account type
            addConstr accExpr taccInfo taccInfo'
            case tassetInfo of
              TypeInfo (TAsset ta) torig tloc -> do
                -- We construct the type of the value that this prim op returns
                -- and then constrain the type variable return type to be this type
                let retTypeInfo' = TypeInfo
                      (holdingsType ta)
                      (InferredFromAssetType primNm (TAsset ta))
                      tloc
                addConstr le retTypeInfo' retTypeInfo
              TypeInfo t torig tloc -> do
                -- If the type is not an asset type, throw a type error
                let typeError = InvalidArgType primNm t (ttype tassetAnyInfo)
                void $ throwErrInferM typeError tloc

          TransferHoldings -> do
            let [accExpr, assetAnyExpr, varExpr, acc2Expr]      = argExprs
                [tacc1Info, tassetAnyInfo, tvarInfo, tacc2Info] = argTypeInfos
                [tacc1Info', tassetInfo, tbalInfo, tacc2Info']  = argExprTypeInfos
            -- add constraint for 1st arg to be an account
            addConstr accExpr tacc1Info tacc1Info'
            -- add constraint for 2nd arg to be an asset
            addConstr assetAnyExpr tassetAnyInfo tassetInfo
            -- add constraint for 3rd arg depending on asset type
            tcHoldingsType varExpr (tassetInfo,tassetAnyInfo) (tbalInfo,tvarInfo)
            -- add constraint for 4th arg to be an account
            addConstr acc2Expr tacc2Info tacc2Info'

          TransferTo       -> do
            let [assetAnyExpr, varExpr]  = argExprs
                [tassetAnyInfo, tvarInfo] = argTypeInfos
                [tassetInfo, tbalInfo]    = argExprTypeInfos
            -- add constraint for 1st arg to be an asset
            addConstr assetAnyExpr tassetAnyInfo tassetInfo
            -- add constraint for 2nd arg depending on asset type
            tcHoldingsType varExpr (tassetInfo,tassetAnyInfo) (tbalInfo,tvarInfo)

          TransferFrom     -> do
            let [assetAnyExpr, varExpr, accExpr]    = argExprs
                [tassetAnyInfo, tvarInfo, taccInfo] = argTypeInfos
                [tassetInfo, tbalInfo, taccInfo']   = argExprTypeInfos
            -- add constraint for 1st arg to be an asset
            addConstr assetAnyExpr tassetAnyInfo tassetInfo
            -- add constraint for 2nd arg depending on asset type
            tcHoldingsType varExpr (tassetInfo,tassetAnyInfo) (tbalInfo, tvarInfo)
            -- add constraint for 3rd arg to be an account
            addConstr accExpr taccInfo taccInfo'

          CirculateSupply -> do
            let [assetAnyExpr, varExpr]  = argExprs
                [tassetAnyInfo, tvarInfo] = argTypeInfos
                [tassetInfo, tbalInfo]    = argExprTypeInfos
            -- add constraint for 1st arg to be an asset
            addConstr assetAnyExpr tassetAnyInfo tassetInfo
            -- add constraint for 2nd arg depending on asset type
            tcHoldingsType varExpr (tassetInfo,tassetAnyInfo) (tbalInfo, tvarInfo)

      CollPrimOp collPrimOp -> do

        -- However, we do some ad-hoc typechecking depending on the collection
        -- prim-op and which collection it's operating over.
        case collPrimOp of
          Aggregate -> do
            let [_, accumExpr, _]  = argExprs
                [_, tinfoAccum, _] = argTypeInfos
                [_, _, tinfoColl'] = argExprTypeInfos
            case tinfoAccum of
              -- The second arg must be a function, and we need to generate
              -- constraints using its arguments so we must pattern match on it.
              TypeInfo (TFun [_, arg2Type] _) torigFunc tLocFunc -> do
                -- Add a constraint such that the 2nd argument of the aggregate
                -- function matches the type of the values in the collection
                addHofArgConstr accumExpr (torigFunc,tLocFunc) tinfoColl' arg2Type
              -- If the type is not a collection type, don't do anything as it
              -- will fail with a unification error later phase.
              TypeInfo t torig tloc -> pure ()

          Transform -> do
            let [funcExpr, _]   = argExprs
                [tinfoFunc, _]  = argTypeInfos
                [tinfoFunc', tinfoColl'] = argExprTypeInfos
            case tinfoFunc of
              -- The first arg must be a function, and we need to generate
              -- constraints using its arguments so we must pattern match on it.
              TypeInfo (TFun [argType] _) torigFunc tLocFunc -> do
                -- Add a constraint such that the only argument of the
                -- function passed to transform matches the type of the
                -- values in the collection.
                addHofArgConstr funcExpr (torigFunc, tLocFunc) tinfoColl' argType
                case tinfoFunc' of
                  TypeInfo (TFun [_] retType) torigFunc' tLocFunc' -> do
                    -- Add a constraint that the type of the values in the
                    -- returned collection match the return type of the hof
                    -- given as an argument to 'transform'.
                    addRetTypeConstr funcExpr (torigFunc', tLocFunc') tinfoColl' retType retTypeInfo
                  -- If the type is not a function type, don't do anything as it
                  -- will fail with a unification error later phase.
                  TypeInfo t torig tloc -> pure ()
              -- If the type is not a function type, don't do anything as it
              -- will fail with a unification error later phase.
              TypeInfo t torig tloc -> pure ()

          Filter -> do
            let [funcExpr, _]   = argExprs
                [tinfoFunc, _]  = argTypeInfos
                [tinfoFunc', tinfoColl'] = argExprTypeInfos
            case tinfoFunc of
              -- The first arg must be a function, and we need to generate
              -- a constraint using its arguments so we must pattern match on it.
              TypeInfo (TFun [argType] _) torigFunc tLocFunc -> do
                -- Add a constraint for the type of argument of the
                -- function to match the type of values in the collection
                addHofArgConstr funcExpr (torigFunc, tLocFunc) tinfoColl' argType
              -- If the type is not a function type, don't do anything as it
              -- will fail with a unification error later phase.
              TypeInfo t torig tloc -> pure ()

          Element -> do
            let [valExpr, _]  = argExprs
                [tinfoVal, _] = argTypeInfos
                [_, tinfoColl'] = argExprTypeInfos
            case tinfoColl' of
              TypeInfo (TColl tcoll) torigCol tlocCol -> do
                let expectedValType =
                      case tcoll of
                        TMap _ vType -> vType
                        TSet vType   -> vType
                    tinfoValExpected = TypeInfo expectedValType (InferredFromCollType primNm tcoll) tlocCol
                -- Add a constraint for the type of value in question to match the type of values
                -- in the collection
                addConstr valExpr tinfoValExpected tinfoVal
              -- If the type is not a function type, don't do anything as it
              -- will fail with a unification error later phase.
              TypeInfo t torig tloc -> pure ()

          -- There is no special typechecking to do here
          IsEmpty -> pure ()

        -- After the ad-hoc constraints are generated, generate constraints for
        -- the type sig for the primops and it's arguments.
        zipWith3M_ addConstr argExprs argTypeInfos argExprTypeInfos

      -- All other primops are typechecked simply-- The expressions supplied as
      -- arguments must unify with the types of the arguments denoted in the prim
      -- op signature.
      normalPrimOp -> zipWith3M_ addConstr argExprs argTypeInfos argExprTypeInfos

    -- The return type of all prim ops has either been contrained in the above
    -- code, or is a monomorphic type that is what it is, and should be the
    -- return type of the prim op as dictated by `primSig`
    return retTypeInfo

  where
    primNm = primName prim

    holdingsType :: TAsset -> Type
    holdingsType TDiscrete       = TInt
    holdingsType TBinary         = TBool
    holdingsType (TFractional p) = TFixed p

    -- Decide on how to constrain the tholdingsInfo argument depending on the
    -- type of the tassetInfo argument (the asset type) supplied to this prim op.
    tcHoldingsType :: LExpr -> (TypeInfo, TypeInfo) -> (TypeInfo, TypeInfo) -> InferM ()
    tcHoldingsType varExpr (tassetInfo,tassetAnyInfo) (tholdingsInfo,tvarInfo) =
      case tassetInfo of
        TypeInfo (TAsset ta) torig tloc ->
          -- If the asset arg is actually an asset type, constrain the type of
          -- the balance arg depending on it's holdings type.
          addConstr varExpr tholdingsInfo $ tvarInfo
            { ttype = holdingsType ta, torig = InferredFromAssetType primNm (TAsset ta) }
        TypeInfo t torig tloc -> do
          -- If the type is not an asset type, throw a type error
          let typeError = InvalidArgType primNm t (ttype tassetAnyInfo)
          void $ throwErrInferM typeError tloc

    -----------------------------------------------------
    -- Helpers for Constraint Gen for Collection PrimOps
    -----------------------------------------------------

    -- The first, second, and first argument of the HO functions aggregate,
    -- transform, and filter must match the type of the values in the collection.
    addHofArgConstr :: LExpr -> (TypeOrigin, Loc) -> TypeInfo -> Type -> InferM ()
    addHofArgConstr hofExpr (hofTypeOrig, hofTypeLoc) tinfoColl argType =
      case tinfoColl of
        TypeInfo (TColl tcoll) _ tlocCol -> do
          let tinfoCollVals vType = TypeInfo vType (InferredFromCollType primNm tcoll) tlocCol
          -- Depending on the collection type, diff constraints are generated...
          case tcoll of
            -- For maps, the value type must unify with the second function arg type.
            TMap _ vType -> addConstr hofExpr (tinfoCollVals vType) tinfoArg
            TSet vType   -> addConstr hofExpr (tinfoCollVals vType) tinfoArg
        -- If the type is not a collection type, don't do anything as it
        -- will fail with a unification error later phase.
        TypeInfo t torig tloc -> pure ()
      where
        tinfoArg = TypeInfo argType hofTypeOrig hofTypeLoc

    -- Add a constraint on the return type of the prim op to be the new type of
    -- the collection after applying the 'transform' primop.
    addRetTypeConstr :: LExpr -> (TypeOrigin, Loc) -> TypeInfo -> Type -> TypeInfo -> InferM ()
    addRetTypeConstr hofExpr (torigHof, tlocHof) tinfoColl newValType retTypeInfo =
      case tinfoColl of
        TypeInfo (TColl tcoll) torigCol tlocCol -> do
          let newCollType =
                case tcoll of
                  TMap kType _ -> TMap kType newValType
                  TSet _       -> TSet newValType
          -- Unify the return type of the whole primop
          -- expression with the map type resulting from the
          -- transformation function.
          let retCollTypeInfo = TypeInfo (TColl newCollType) torigHof tlocHof
          addConstr hofExpr retTypeInfo retCollTypeInfo
        -- If the type is not an collection type, don't do anything as
        -- it will fail with a unification error.
        TypeInfo t torig tloc -> pure ()

-- | Type signatures of builtin primitive operations.
primSig :: PrimOp -> InferM Sig
primSig = \case
  Verify              -> pure $ Sig [TAccount, TMsg, TSig] TBool
  Sign                -> pure $ Sig [TMsg] TSig
  Block               -> pure $ Sig [] TInt
  Deployer            -> pure $ Sig [] TAccount
  Sender              -> pure $ Sig [] TAccount
  Bound               -> pure $ Sig [TAssetAny, TAccount] TBool
  Created             -> pure $ Sig [] TDateTime
  Address             -> pure $ Sig [] TContract
  Validator           -> pure $ Sig [] TAccount
  Sha256              -> pure $ Sig [TAny] TMsg
  AccountExists       -> pure $ Sig [TAccount] TBool
  AssetExists         -> pure $ Sig [TAssetAny] TBool
  ContractExists      -> pure $ Sig [TContract] TBool
  Terminate           -> pure $ Sig [TMsg] TAny
  Now                 -> pure $ Sig [] TDateTime
  Transition          -> pure $ Sig [TState] TVoid
  CurrentState        -> pure $ Sig [] TState
  TxHash              -> pure $ Sig [] TMsg
  ContractValue       -> Sig [TContract, TMsg]   <$> freshTVar
  ContractValueExists -> pure $ Sig [TContract, TMsg] TBool
  ContractState       -> pure $ Sig [TContract] TState
  NovationInit        -> pure $ Sig [TInt] TVoid
  NovationStop        -> pure $ Sig [] TVoid
  IsBusinessDayUK     -> pure $ Sig [TDateTime] TBool
  NextBusinessDayUK   -> pure $ Sig [TDateTime] TDateTime
  IsBusinessDayNYSE   -> pure $ Sig [TDateTime] TBool
  NextBusinessDayNYSE -> pure $ Sig [TDateTime] TDateTime
  Between             -> pure $ Sig [TDateTime, TDateTime, TDateTime] TDateTime
  TimeDiff            -> pure $ Sig [TDateTime, TDateTime] TTimeDelta
  Fixed1ToFloat       -> pure $ Sig [TFixed Prec1] TFloat
  Fixed2ToFloat       -> pure $ Sig [TFixed Prec2] TFloat
  Fixed3ToFloat       -> pure $ Sig [TFixed Prec3] TFloat
  Fixed4ToFloat       -> pure $ Sig [TFixed Prec4] TFloat
  Fixed5ToFloat       -> pure $ Sig [TFixed Prec5] TFloat
  Fixed6ToFloat       -> pure $ Sig [TFixed Prec6] TFloat
  FloatToFixed1       -> pure $ Sig [TFloat] (TFixed Prec1)
  FloatToFixed2       -> pure $ Sig [TFloat] (TFixed Prec2)
  FloatToFixed3       -> pure $ Sig [TFloat] (TFixed Prec3)
  FloatToFixed4       -> pure $ Sig [TFloat] (TFixed Prec4)
  FloatToFixed5       -> pure $ Sig [TFloat] (TFixed Prec5)
  FloatToFixed6       -> pure $ Sig [TFloat] (TFixed Prec6)
  AssetPrimOp a       -> assetPrimSig a
  MapPrimOp m         -> mapPrimSig m
  SetPrimOp m         -> setPrimSig m
  CollPrimOp c        -> collPrimSig c

assetPrimSig :: AssetPrimOp -> InferM Sig
assetPrimSig = \case
  HolderBalance    -> Sig [TAssetAny, TAccount] <$> freshTAVar
  TransferHoldings -> freshTAVar >>= \tav -> pure $ Sig [TAccount, TAssetAny, tav, TAccount] TVoid -- from Account to Account
  TransferTo       -> freshTAVar >>= \tav -> pure (Sig [TAssetAny, tav] TVoid)                     -- from Account to Contract
  TransferFrom     -> freshTAVar >>= \tav -> pure (Sig [TAssetAny, tav, TAccount] TVoid)           -- from Contract to Account
  CirculateSupply  -> freshTAVar >>= \tav -> pure (Sig [TAssetAny, tav] TVoid)                     -- from Asset Supply to Asset issuer's holdings

mapPrimSig :: MapPrimOp -> InferM Sig
mapPrimSig = \case
  MapInsert -> do
    a <- freshTVar
    b <- freshTVar
    pure $ Sig [a, b, TColl (TMap a b)] (TColl (TMap a b))
  MapDelete -> do
    a <- freshTVar
    b <- freshTVar
    pure $ Sig [a, TColl (TMap a b)] (TColl (TMap a b))
  MapLookup -> do
    a <- freshTVar
    b <- freshTVar
    pure $ Sig [a, TColl (TMap a b)] b
  MapModify -> do
    a <- freshTVar
    b <- freshTVar
    pure $ Sig [a, TFun [b] b, TColl (TMap a b)] (TColl (TMap a b))

setPrimSig :: SetPrimOp -> InferM Sig
setPrimSig = \case
  SetInsert -> do
    a <- freshTVar
    pure $ Sig [a, TColl (TSet a)] (TColl (TSet a))
  SetDelete -> do
    a <- freshTVar
    pure $ Sig [a, TColl (TSet a)] (TColl (TSet a))

collPrimSig :: CollPrimOp -> InferM Sig
collPrimSig = \case
  Aggregate -> do
    a <- freshTVar
    b <- freshTVar
    c <- freshTCVar
    pure $ Sig [a, TFun [a,b] a, c] a
  Transform -> do
    a <- freshTVar
    b <- freshTVar
    c <- freshTCVar
    d <- freshTCVar
    pure $ Sig [TFun [a] b, c] d
  Filter -> do
    a <- freshTVar
    b <- freshTCVar
    c <- freshTCVar
    pure $ Sig [TFun [a] TBool, b] c
  Element -> do
    a <- freshTVar
    b <- freshTCVar
    pure $ Sig [a, b] TBool
  IsEmpty -> do
    a <- freshTCVar
    pure $ Sig [a] TBool

-------------------------------------------------------------------------------
-- Valid Binary Op logic
-------------------------------------------------------------------------------

tcUnOp :: LUnOp -> LExpr -> InferM TypeInfo
tcUnOp (Located opLoc op) e = do
    tcUnOp' opLoc (UnaryOperator op) e
  where
    tcUnOp' =
      case op of
        Not -> tcNotOp

tcBinOp :: LBinOp -> LExpr -> LExpr -> InferM TypeInfo
tcBinOp (Located opLoc op) e1 e2 = do
    tcBinOp' opLoc (BinaryOperator op) e1 e2
  where
    tcBinOp' =
      case op of
        Mul -> tcMult
        Add -> tcAddSub op
        Sub -> tcAddSub op
        Div -> tcDiv
        And -> tcAndOr
        Or  -> tcAndOr
        Equal   -> tcEqual Equal
        LEqual  -> tcLEqual LEqual
        GEqual  -> tcGEqual
        Greater -> tcGreater
        Lesser  -> tcLesser
        NEqual  -> tcNEqual

-- | Multiplication is only valid for:
--     TDelta * TInt
--     TInt * TDelta
--     TInt   * TInt
--     TFloat * TFloat
--     TFixed * TFixed
tcMult :: Loc -> TypeOrigin -> LExpr -> LExpr -> InferM TypeInfo
tcMult opLoc torig e1 e2 = do
  tinfo1 <- tcLExpr e1
  tinfo2 <- tcLExpr e2
  case (ttype tinfo1, ttype tinfo2) of

    -- The reason for these alternating pattern matches is to correctly
    -- typecheck unusual binops in which both expression the operation is over
    -- do not need to necessarily have the same type, albeit a specific one.
    (TTimeDelta, _)   -> addConstrAndRetInfo' (tDeltaInfo torig eLoc) (tIntInfo torig opLoc, tinfo2)
    (_, TTimeDelta)   -> addConstrAndRetInfo' (tDeltaInfo torig eLoc) (tinfo1, tIntInfo torig opLoc)

    (TInt, _)         -> addConstrAndRetInfo' (tIntInfo torig eLoc) (tIntInfo torig opLoc, tinfo2)
    (TFloat, _)       -> addConstrAndRetInfo' (tFloatInfo torig eLoc) (tFloatInfo torig opLoc, tinfo2)
    (TFixed p, _)     -> addConstrAndRetInfo' (tFixedInfo p torig eLoc) (tFixedInfo p torig opLoc, tinfo2)
    (TVar a, _ )      -> do
      tinfo' <- TypeInfo <$> freshTVar <*> pure torig <*> pure eLoc
      addConstrAndRetInfo' tinfo' (tinfo1, tinfo2)

    (t1,t2)           -> do
      throwErrInferM (InvalidBinOp Mul t1 t2) opLoc
      return $ TypeInfo TError torig eLoc
  where
    eLoc = located e1
    addConstrAndRetInfo' = addConstrAndRetInfo e2

-- | Add, Sub is only valid for:
--     TInt           +/- TInt
--     TFloat         +/- Float
--     TFixed         +/- TFixed
--     TDatetime      +/- TDelta
--     TDelta          +  TDelta
--     TMsg            +  TMsg (concatenation)
tcAddSub :: BinOp -> Loc -> TypeOrigin -> LExpr -> LExpr -> InferM TypeInfo
tcAddSub op opLoc torig e1 e2 = do
  tinfo1 <- tcLExpr e1
  tinfo2 <- tcLExpr e2
  case (ttype tinfo1, ttype tinfo2) of

    -- Constrain the LHS to be the only type expected in the binary operation
    -- that it can be, due to binary op definitions in FCL.
    (TInt, _)         -> addConstrAndRetInfo' (tIntInfo torig eLoc) (tIntInfo torig opLoc, tinfo2)
    (TFloat, _)       -> addConstrAndRetInfo' (tFloatInfo torig eLoc) (tFloatInfo torig opLoc, tinfo2)
    (TFixed p, _)     -> addConstrAndRetInfo' (tFixedInfo p torig eLoc) (tFixedInfo p torig opLoc, tinfo2)
    (TDateTime, _)    -> addConstrAndRetInfo' (tDatetimeInfo torig eLoc) (tDeltaInfo torig opLoc, tinfo2)
    -- TDelta + TDelta (no subtraction)
    (TTimeDelta, _)   -> tcAddNoSub tinfo1 tinfo2
    -- TMsg + TMsg (concatenation, no subtraction)
    (TMsg, _)         -> tcAddNoSub tinfo1 tinfo2
    (TVar a, _)       -> do
      tinfo' <- TypeInfo <$> freshTVar <*> pure torig <*> pure eLoc
      addConstrAndRetInfo' tinfo' (tinfo1, tinfo2)
    (t1, t2)          -> do
      throwErrInferM (InvalidBinOp op t1 t2) opLoc
      return $ TypeInfo TError torig eLoc
  where
    eLoc = located e1
    addConstrAndRetInfo' = addConstrAndRetInfo e2

    tcAddNoSub tinfo1 tinfo2
      | op == Sub = do
          throwErrInferM (InvalidBinOp op constructor typ) opLoc
          return $ TypeInfo TError torig eLoc
      | otherwise = addConstrAndRetInfo' (tinfoMaker torig eLoc) (tinfo1, tinfoMaker torig opLoc)
      where
        (typ, (tinfoMaker, constructor)) =
          (ttype tinfo2,) $ case ttype tinfo1 of
            TMsg       -> (tMsgInfo, TMsg)
            TTimeDelta -> (tDeltaInfo, TTimeDelta)
            otherwise  -> panic "The only invalid addition-no-subtraction types are TMsg and TTimeDelta"

-- | Division is only valid for:
--     TInt   / TInt
--     TFloat / TFLoat
--     TFixed / TFixed
tcDiv :: Loc -> TypeOrigin -> LExpr -> LExpr -> InferM TypeInfo
tcDiv opLoc torig e1 e2 = do
  tinfo1 <- tcLExpr e1
  tinfo2 <- tcLExpr e2
  case (ttype tinfo1, ttype tinfo2) of
    (TInt, _)         -> addConstrAndRetInfo' (tIntInfo torig eLoc) (tIntInfo torig opLoc, tinfo2)
    (TFloat, _)       -> addConstrAndRetInfo' (tFloatInfo torig eLoc) (tFloatInfo torig opLoc, tinfo2)
    (TFixed p, _)     -> addConstrAndRetInfo' (tFixedInfo p torig eLoc) (tFixedInfo p torig opLoc, tinfo2)
    (TVar a, _ )  -> do
      tinfo' <- TypeInfo <$> freshTVar <*> pure torig <*> pure eLoc
      addConstrAndRetInfo' tinfo' (tinfo1, tinfo2)
    (t1,t2)           -> do
      throwErrInferM (InvalidBinOp Div t1 t2) opLoc
      return $ TypeInfo TError torig eLoc
  where
    eLoc = located e1
    addConstrAndRetInfo' = addConstrAndRetInfo e2

tcAndOr :: Loc -> TypeOrigin -> LExpr -> LExpr -> InferM TypeInfo
tcAndOr opLoc torig e1 e2 = do
  tinfo1 <- tcLExpr e1
  tinfo2 <- tcLExpr e2
  let argTypeInfo = TypeInfo TBool torig opLoc
  addConstr e2 argTypeInfo tinfo1
  addConstr e2 argTypeInfo tinfo2
  return $ TypeInfo TBool torig (located e2)

tcEqual :: BinOp -> Loc -> TypeOrigin -> LExpr -> LExpr -> InferM TypeInfo
tcEqual op opLoc torig e1 e2 = do
  tinfo1 <- tcLExpr e1
  tinfo2 <- tcLExpr e2
  case (ttype tinfo1, ttype tinfo2) of
    (TContract, _) -> addConstrAndRetBool (tContractInfo torig opLoc, tinfo2)
    (TInt, _)      -> addConstrAndRetBool (tIntInfo torig opLoc, tinfo2)
    (TFloat, _)    -> addConstrAndRetBool (tFloatInfo torig opLoc, tinfo2)
    (TFixed p, _)  -> addConstrAndRetBool (tFixedInfo p torig opLoc, tinfo2)
    (TAccount, _)  -> addConstrAndRetBool (tAccountInfo torig opLoc, tinfo2)
    (TBool, _)     -> addConstrAndRetBool (tBoolInfo torig opLoc, tinfo2)
    (TAsset at, _) -> addConstrAndRetBool (tAssetInfo at torig opLoc, tinfo2)
    (TDateTime, _) -> addConstrAndRetBool (tDatetimeInfo torig opLoc, tinfo2)
    (TTimeDelta, _) -> addConstrAndRetBool (tDeltaInfo torig opLoc, tinfo2)
    (TMsg, _)      -> addConstrAndRetBool (tMsgInfo torig opLoc, tinfo2)
    (TVar a, _)    -> addConstrAndRetBool (tinfo1, tinfo2)
    (t1,t2)        -> do
      throwErrInferM (InvalidBinOp op t1 t2) opLoc
      return $ tBoolInfo torig eLoc
  where
    eLoc = located e2
    addConstrAndRetBool = addConstrAndRetInfo e2 $ tBoolInfo torig eLoc

tcLEqual :: BinOp -> Loc -> TypeOrigin -> LExpr -> LExpr -> InferM TypeInfo
tcLEqual op opLoc torig e1 e2 = do
  tinfo1 <- tcLExpr e1
  tinfo2 <- tcLExpr e2
  case (ttype tinfo1, ttype tinfo2) of
    (TInt, _)        -> addConstrAndRetBool (tIntInfo torig opLoc, tinfo2)
    (TFloat, _)      -> addConstrAndRetBool (tFloatInfo torig opLoc, tinfo2)
    (TFixed p, _)    -> addConstrAndRetBool (tFixedInfo p torig opLoc, tinfo2)
    (TDateTime, _)   -> addConstrAndRetBool (tDatetimeInfo torig opLoc, tinfo2)
    (TTimeDelta, _)  -> addConstrAndRetBool (tDeltaInfo torig opLoc, tinfo2)
    (TVar a, _)      -> addConstrAndRetBool (tinfo1, tinfo2)
    (t1,t2)          -> do
      throwErrInferM (InvalidBinOp op t1 t2) opLoc
      return $ TypeInfo TError torig eLoc
  where
    eLoc = located e2
    addConstrAndRetBool = addConstrAndRetInfo e2 $ tBoolInfo torig eLoc

tcGEqual :: Loc -> TypeOrigin -> LExpr -> LExpr -> InferM TypeInfo
tcGEqual = tcLEqual GEqual

tcGreater :: Loc -> TypeOrigin -> LExpr -> LExpr -> InferM TypeInfo
tcGreater = tcLEqual Greater

tcLesser :: Loc -> TypeOrigin -> LExpr -> LExpr -> InferM TypeInfo
tcLesser = tcLEqual Lesser

tcNEqual :: Loc -> TypeOrigin -> LExpr -> LExpr -> InferM TypeInfo
tcNEqual = tcEqual NEqual

tcNotOp :: Loc -> TypeOrigin -> LExpr -> InferM TypeInfo
tcNotOp opLoc torig e1 = do
  tinfo <- tcLExpr e1
  case ttype tinfo of
    TBool       -> return $ tBoolInfo torig eLoc
    invalidUnOp -> do
      throwErrInferM (InvalidUnOp Not invalidUnOp) opLoc
      return $ TypeInfo TError torig eLoc
  where
    eLoc = located e1

-- | Helper for common pattern "Add constraint of two TypeInfos and return a TypeInfo"
addConstrAndRetInfo :: LExpr -> TypeInfo -> (TypeInfo, TypeInfo) -> InferM TypeInfo
addConstrAndRetInfo le retInfo (expected, actual) =
  addConstr le expected actual >> return retInfo

-------------------------------------------------------------------------------
-- Contraint Generation
-------------------------------------------------------------------------------

data Constraint = Constraint
  { mOrigLExpr :: Maybe LExpr -- ^ Maybe the expression from which the constraint originated
  , expected :: TypeInfo
  , actual   :: TypeInfo
  } deriving (Show)

-- | Add a constraint during the constraint generation phase
addConstr :: LExpr -> TypeInfo -> TypeInfo -> InferM ()
addConstr lexpr expected' actual' = modify' $ \s ->
  s { constrs = constrs s ++ [constr] }
  where
    constr = Constraint
      { mOrigLExpr = Just lexpr
      , expected = expected'
      , actual   = actual'
      }

instance Pretty Constraint where
  ppr (Constraint mLExpr ti1 ti2) =
    "Constraint:"
    <$$+> (maybe "" (ppr . locVal) mLExpr)
    <$$+> ppr ti1
    <$$+> ppr ti2

instance Pretty [Constraint] where
  ppr [] = ""
  ppr (c:cs) = ppr c <$$> Script.Pretty.line <> ppr cs

-------------------------------------------------------------------------------
-- Inference Utils
-------------------------------------------------------------------------------

typeVars :: [Text]
typeVars = [1..] >>= flip replicateM ['a'..'z'] >>= return . toS

freshTVar :: InferM Type
freshTVar = TVar <$> freshTVar'

freshTVar' :: InferM TVar
freshTVar' = do
  inferState <- get
  put $ inferState { count = count inferState + 1 }
  return $ TV (typeVars `unsafeIndex` count inferState)

freshTAVar :: InferM Type
freshTAVar = TVar <$> freshTAVar'

freshTAVar' :: InferM TVar
freshTAVar' = do
  TV v <- freshTVar'
  pure $ TAV v

freshTCVar :: InferM Type
freshTCVar = TVar <$> freshTCVar'

freshTCVar' :: InferM TVar
freshTCVar' = do
  TV v <- freshTVar'
  pure $ TCV v


lookupVarType :: LName -> InferM (Maybe (TMeta, TypeInfo))
lookupVarType (Located loc name) = do
  (TypeEnv typeEnv) <- varEnv <$> get
  return $ Map.lookup name typeEnv

-- | Just like 'lookupVarType' but throws a type error if the variable doesn't
-- exist in the typing env.
lookupVarType' :: LName -> InferM (TMeta, TypeInfo)
lookupVarType' var@(Located loc name) = do
  mVarTypeInfo <- lookupVarType var
  case mVarTypeInfo of
    Nothing -> (Temp,) <$> throwErrInferM (UnboundVariable name) loc
    Just typeInfo -> return typeInfo

-- | Checks if # args suppltiied to function match # args in Sig
arityCheck :: LName -> [Type] -> [LExpr] -> InferM ()
arityCheck (Located loc nm) typs args
  | lenTyps == lenArgs = return ()
  | otherwise = void $ throwErrInferM (ArityFail nm lenTyps lenArgs) loc
  where
    lenTyps = length typs
    lenArgs = length args

-------------------------------------------------------------------------------
-- Substitution
-------------------------------------------------------------------------------

class Substitutable a where
  apply :: Subst -> a -> a

instance Substitutable Type where
  apply s t@(TVar a)  = Map.findWithDefault t a (unSubst s)
  apply s TError      = TError
  apply s TInt        = TInt
  apply s TFloat      = TFloat
  apply s (TFixed p)  = TFixed p
  apply s TBool       = TBool
  apply s TAccount    = TAccount
  apply s (TAsset at) = TAsset at
  apply s TContract   = TContract
  apply s TMsg        = TMsg
  apply s TVoid       = TVoid
  apply s TSig        = TSig
  apply s TAny        = TAny
  apply s TAssetAny   = TAssetAny
  apply s TState      = TState
  apply s TDateTime   = TDateTime
  apply s (TFun ats rt) = TFun (map (apply s) ats) (apply s rt)
  apply s t@TEnum{}   = t
  apply s TTimeDelta  = TTimeDelta
  apply s (TColl tc)  = TColl (apply s tc)

instance Substitutable TCollection where
  apply s (TMap k v) = TMap (apply s k) (apply s v)
  apply s (TSet v)   = TSet (apply s v)

instance (Substitutable a, Substitutable b) => Substitutable (a,b) where
  apply s (a,b) = (apply s a, apply s b)

instance Substitutable TypeInfo where
  apply s (TypeInfo t info tPos) = TypeInfo (apply s t) info tPos

instance Substitutable TMeta where
  apply s tmeta = tmeta

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply

instance Substitutable Constraint where
  apply s (Constraint e t1 t2) = Constraint e (apply s t1) (apply s t2)

instance Substitutable TypeEnv where
  apply s (TypeEnv varEnv) =  TypeEnv $ Map.map (apply s) varEnv


newtype Subst = Subst { unSubst :: Map.Map TVar Type }

instance Monoid Subst where
  mempty = Subst mempty
  mappend = composeSubst

instance Pretty Subst where
  ppr (Subst s) =
    case Map.toList s of
      [] -> ""
      ((k,v):m') -> ppr k <+> "=>" <+> ppr v
               <$$> ppr (Subst (Map.fromList m'))

composeSubst :: Subst -> Subst -> Subst
composeSubst s@(Subst s1) (Subst s2) =
  Subst (Map.map (apply s) s2 `Map.union` s1)

-------------------------------------------------------------------------------
-- Unification & Solving
-------------------------------------------------------------------------------

data Unifier = Unifier Subst [Constraint]

instance Monoid Unifier where
  mempty = Unifier mempty mempty
  mappend (Unifier subst cs) (Unifier subst' cs') =
    Unifier (subst' `composeSubst` subst) (cs' ++ apply subst' cs)

instance Monoid (Either a Unifier) where
  mempty = Right mempty
  -- Note: Short circuits on Left, does not accumulate values.
  mappend (Left e) _           = Left e
  mappend _  (Left e)          = Left e
  mappend (Right u) (Right u') = Right (u <> u')

instance Pretty Unifier where
  ppr (Unifier subst cs) = "Unifier:"
               <$$+> ("Subst:" <$$+> ppr subst)
               <$$+> ("Constraints:" <$$+> ppr cs)

type SolverM = State [TypeError]

runSolverM :: [TypeError] -> [Constraint] -> Either (NonEmpty TypeError) Subst
runSolverM typeErrs constrs =
    case nonEmpty unifErrs of
      Nothing   -> Right subst
      Just errs -> Left errs
  where
    (subst, unifErrs) =
      flip runState typeErrs $ solver (Unifier mempty constrs)

throwErrSolverM :: TypeError -> SolverM Unifier
throwErrSolverM typeErr = do
  modify' $ flip (++) [typeErr]
  return mempty

bind ::  TVar -> Type -> Unifier
bind tv t = Unifier (Subst (Map.singleton tv t)) []

unify :: Constraint -> SolverM Unifier
unify (Constraint mLExpr to1 to2) =
    case unify' t1 t2 of
      Left tErrInfo -> throwErrSolverM $ TypeError tErrInfo terrLoc
      Right unifier -> return unifier
  where
    TypeInfo t1 t1orig t1Pos = to1
    TypeInfo t2 t2orig t2Pos = to2

    terrLoc = maybe t2Pos located mLExpr

    unify' :: Type -> Type -> Either TypeErrInfo Unifier
    unify' t1 t2 | t1 == t2          = Right mempty
    unify' TAny   _                  = Right mempty
    unify' _      TAny               = Right mempty
    unify' (TVar v) t                = unifyTVar t v
    unify' t (TVar v)                = unifyTVar t v
    unify' TAssetAny (TAsset ta)     = Right mempty
    unify' (TAsset ta) TAssetAny     = Right mempty
    unify' TError t                  = Right mempty
    unify' t TError                  = Right mempty
    unify' (TColl tc1) (TColl tc2)   = unifyTColl tc1 tc2
    unify' (TFun as r) (TFun as' r') = unifyTFun (as,r) (as',r')
    unify' t1 t2                     = Left $ UnificationFail to1 to2

    -- Generate unifiers for all arguments/return types for both functions. Here
    -- we do not generate constraints as per usual unification of functions for
    -- a bit more explicit type error (reporting that the function types don't
    -- unify, rather than the individual arg/ret type pairs don't unify).
    unifyTFun :: ([Type], Type) -> ([Type], Type) -> Either TypeErrInfo Unifier
    unifyTFun (as,r) (as',r')
      | length as == length as' =
          foldMap (uncurry unify') (zip (as ++ [r]) (as' ++ [r']))
        -- Right (mempty, constrs)
      | otherwise = Left $ UnificationFail to1 to2 -- Fail because of arity mismatch

    unifyTColl :: TCollection -> TCollection -> Either TypeErrInfo Unifier
    unifyTColl (TMap k1 v1) (TMap k2 v2) = unify' k1 k2 >> unify' v1 v2
    unifyTColl (TSet v1)    (TSet v2)    = unify' v1 v2
    unifyTColl _            _            = Left $ UnificationFail to1 to2

    unifyTVar :: Type -> TVar -> Either TypeErrInfo Unifier
    -- TV (general type vars) unify with anything
    unifyTVar t            v@(TV _)  = Right $ v `bind` t
    -- TAV (asset specific type vars) unify with only 3 types
    unifyTVar TInt         v@(TAV _) = Right $ v `bind` TInt
    unifyTVar TBool        v@(TAV _) = Right $ v `bind` TBool
    unifyTVar t@(TFixed _) v@(TAV _) = Right $ v `bind` t
    unifyTVar _            v@(TAV _) = Left $ UnificationFail to1 to2
    -- TCV (collection specific type vars)
    unifyTVar t@(TColl _)      v@(TCV c) = Right $ v `bind` t
    unifyTVar t@(TVar (TCV _)) v@(TCV c) = Right $ v `bind` t -- TCVar binds with TCVar
    unifyTVar _                v@(TCV _) = Left $ UnificationFail to1 to2


solver :: Unifier -> SolverM Subst
solver (Unifier subst initConstrs) =
  case initConstrs of
    [] -> return subst
    (c:cs) -> do
      resUnifier <- unify c
      let processedUnifier = Unifier subst cs
      -- Debugging:
      -- traceM (prettyPrint $ processedUnifier <> resUnifier)
      solver (processedUnifier <> resUnifier)

-------------------------------------------------------------------------------
-- Pretty Printer
-------------------------------------------------------------------------------

instance Pretty Sig where
  ppr (Sig argtys TVoid) = tupleOf argtys <+> "->" <+> "()"
  ppr (Sig argtys retty) = tupleOf argtys <+> "->" <+> ppr retty

instance Pretty TypeOrigin where
  ppr torig = case torig of
    OutOfThinAir        -> "is a fresh type variable"
    VariableDefn nm     -> "was inferred by top level definition for variable" <+> squotes (ppr nm)
    InferredFromVar nm  -> "was inferred variable" <+> squotes (ppr nm)
    InferredFromLit lit -> "was inferred from literal" <+> squotes (ppr lit)
    InferredFromExpr e  -> "was inferred from expression" <+> squotes (ppr e)
    InferredFromAssetType nm t -> "was inferred from the asset type" <+> squotes (ppr t) <+> "supplied as an argument to the primop" <+> squotes (ppr nm)
    InferredFromHelperDef nm -> "was inferred from the helper function definition" <+> squotes (ppr nm)
    InferredFromAssignment nm -> "was inferred from the assignment to variable" <+> squotes (ppr nm)
    BinaryOperator op   -> "was inferred from use of binary operator" <+> squotes (ppr op)
    UnaryOperator op    -> "was inferred from use of unary operator" <+> squotes (ppr op)
    Assignment          -> "was inferred from variable assignment"
    IfCondition         -> "must be a bool because of if statement"
    DateTimeGuardPred   -> "must be a datetime because it is a datetime guard predicate"
    DateTimeGuardBody   -> "must be a void because it is the body of a datetime guard"
    FunctionArg n nm    -> "was inferred from the type signature of argument" <+> ppr n <+> "of function" <+> squotes (ppr nm)
    FunctionRet nm      -> "was inferred from the return type of the function" <+> squotes (ppr nm)
    CasePattern nm      -> "was inferred from type of case pattern" <+> squotes (ppr nm)
    CaseBody e          -> "was inferred from type of case body" <+> squotes (ppr e)
    InferredFromCollType nm t -> "was inferred from the collection type" <+> squotes (ppr t) <+> "supplied as an argument to the primop" <+> squotes (ppr nm)

instance Pretty TypeInfo where
  ppr (TypeInfo t orig loc) = "Type" <+> ppr t <+> ppr orig <+> "on" <+> ppr loc

instance Pretty TypeErrInfo where
  ppr e = case e of
    UnboundVariable nm            -> "Unbound variable: " <+> ppr nm
    InvalidDefinition nm e lhsTyp rhsTyp
                                  -> "Invalid definition for" <+> ppr nm <> ":"
                                  <$$+> "Expected type:" <+> ppr lhsTyp
                                  <$$+> "But inferred type:" <+> ppr rhsTyp <+> "for expression" <+> squotes (ppr e)
    UndefinedFunction nm          -> "Invalid function name: " <+> ppr nm
    InvalidBinOp op t1 t2         -> "Invalid binary operation: "
                                  <$$+> squotes (ppr op) <+> "does not accept types" <+> ppr t1 <+> "and" <+> ppr t2
    InvalidUnOp op t              -> "Invalid unary operation: "
                                  <$$+> squotes (ppr op) <+> "does not accept types" <+> ppr t
    InvalidAddress nm             -> "Invalid address: " <+> ppr nm
                                  <$$+> "Addresses must be a valid base 58 encoded sha256 hashes."
    ArityFail nm n m              -> "Arity mismatch in function call" <+> ppr nm <> ":"
                                  <$$+> "Expecting" <+> ppr n <+> "arguments, but got" <+> ppr m
    InvalidArgType nm t1 t2       -> "Invalid argument type to method" <+> ppr nm <> ":"
                                  <$$+> "Expecting type" <+> ppr t1 <+> "but got" <+> ppr t2
    VarNotFunction nm t           -> "Variable" <+> squotes (ppr nm) <+> "is not a helper function"
                                  <$$+> "Expecting a function type, but got" <+> squotes (ppr t)
    UnificationFail tinfo1 tinfo2 -> "Could not match expected type"
                                  <+> squotes (ppr $ ttype tinfo1)
                                  <+> "with actual type"
                                  <+> squotes (ppr $ ttype tinfo2) <> ":"
                                  <$$+> ppr tinfo1
                                  <$$+> ppr tinfo2
    InvalidLocalVarAssign t tinfo -> "Invalid local var assignment:"
                                  <$$+> ppr t
                                  <$$+> ppr tinfo
    CaseOnNotEnum e               -> "Case analysis on a non-enum type:"
                                  <$$+> ppr e
    UnknownConstructor c          -> "Reference to undefined constructor:"
                                  <$$+> ppr c
    UnknownEnum e                 -> "Reference to undefined enum type:"
                                  <$$+> ppr e
    PatternMatchError misses dups -> "Pattern match failures:"
                                  <$$+> vsep (map ((" - Missing case for:" <+>) . ppr) misses)
                                  <$$+> vsep (map ((" - Duplicate case for:" <+>) . ppr) dups)
    EmptyMatches                  -> "Case expression with no matches"
    InvalidAccessRestriction e t  -> ("Invalid access restriction:" <$$+> ppr e)
                                  <$$> "Expected type"
                                  <+> ppr TAccount
                                  <+> "but got"
                                  <+> ppr t
    Impossible msg                -> "The impossible happened:" <+> ppr msg

instance Pretty TypeError where
  ppr (TypeError tErrInfo tPos) = "Type error at" <+> ppr tPos <> ":"
                             <$$+> ppr tErrInfo

instance Pretty [TypeError] where
  ppr es = case map ppr (sort es) of
    [] -> ""
    (e:es) -> foldl' (<$$$>) (ppr e) $ map ppr es

instance Pretty (NonEmpty TypeError) where
  ppr = ppr . NonEmpty.toList

-- | Pretty print a type error
ppError :: TypeError -> LText
ppError = render . ppr

-- | Pretty print a type signature
ppSig :: Sig -> LText
ppSig = render . ppr
