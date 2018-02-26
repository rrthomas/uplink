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
  lookupSig,
  tcMethod,

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
import Address (Address, validateAddress)
import Utils (duplicates)
import Asset (AssetType(..))

import Control.Monad.Writer
import Control.Monad.State.Strict (modify')

import Data.Hashable
import Data.List (lookup)
import qualified Data.List as List
import Data.Serialize (Serialize)
import qualified Data.Map as Map

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
  | InvalidDefinition Name Type Lit     -- ^ Invalid definition
  | InvalidUnOp UnOp Type               -- ^ Invalid unary op
  | InvalidBinOp BinOp Type Type        -- ^ Invalid binary op
  | InvalidPrimOp Name                  -- ^ Invocation of non-primop function
  | InvalidReturnType                   -- ^ Invalid return value (cannot return locals)
  | InvalidAddress Address              -- ^ Invalid address
  | InvalidArgType Name Type Type       -- ^ Invalid argument to Method call
  | InvalidLocalVarAssign ByteString TypeInfo -- ^ Invalid local variable assignment
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
  | Impossible ByteString               -- ^ Malformed syntax, impossible
  deriving (Eq, Show, Generic, Serialize)

-- | Type error
data TypeError = TypeError
  { errInfo :: TypeErrInfo
  , errLoc  :: Loc
  } deriving (Eq, Show, Generic, Serialize)

instance Ord TypeError where
  compare te1 te2 = compare (errLoc te1) (errLoc te2)

-- | Source of type error
data TypeOrigin
  = OutOfThinAir
  | VariableDefn Name     -- ^ Top level definitions
  | InferredFromVar Name  -- ^ Local method variable assignment
  | InferredFromExpr Expr -- ^ Local method variable assignment
  | InferredFromLit Lit   -- ^ Literal types
  | InferredFromAssetType Name Type -- ^ Holdings type inferred from asset type passed to primop
  | UnaryOperator UnOp    -- ^ From unary operation
  | BinaryOperator BinOp  -- ^ From binary operation
  | IfCondition           -- ^ From if condition
  | DateTimeGuardPred     -- ^ From DateTime guard predicate
  | DateTimeGuardBody     -- ^ From DateTime guard body
  | Assignment            -- ^ From var assignment
  | ArgToMethod Name      -- ^ Var passed as method arg
  | PrimOpArg Name        -- ^ Passed to prim op
  | PrimOpRet Name        -- ^ Returned from prip op
  | CasePattern Name      -- ^ Enum type of pattern
  | CaseBody Expr         -- ^ Body of case match
  deriving (Eq, Show, Generic, Serialize)

-- | Type error metadata
data TypeInfo = TypeInfo
  { ttype :: Type        -- ^ What type
  , torig :: TypeOrigin  -- ^ Where did it come from
  , tloc  :: Loc         -- ^ Where is it located
  } deriving (Show, Eq, Generic, Serialize)

tCryptoInfo, tIntInfo, tFloatInfo, tContractInfo, tBoolInfo, tAccountInfo, tDatetimeInfo, tDeltaInfo, tMsgInfo :: TypeOrigin -> Loc -> TypeInfo
tCryptoInfo = TypeInfo (TCrypto TInt)
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
  , env     :: TypeEnv
  } deriving (Show)

initInferState :: InferState
initInferState = InferState
  { count   = 0
  , errs    = []
  , constrs = []
  , env     = emptyEnv
  }

data TMeta = Local | Global | Temp
  deriving (Show)

newtype TypeEnv = TypeEnv (Map Name (TMeta, TypeInfo))
  deriving (Show)

extendEnv :: TypeEnv -> (Name, TMeta, TypeInfo) -> TypeEnv
extendEnv (TypeEnv env) (x, tmeta, typeInfo) =
  TypeEnv (Map.insert x (tmeta, typeInfo) env)

extendEnvM :: (Name, TMeta, TypeInfo) -> InferM ()
extendEnvM v = modify' $ \s ->
  s { env = extendEnv (env s) v }

removeEnv :: TypeEnv -> Name -> TypeEnv
removeEnv (TypeEnv env) x = TypeEnv (Map.delete x env)

removeEnvM :: Name -> InferM ()
removeEnvM nm = modify' $ \s ->
  s { env = removeEnv (env s) nm }

unionTypeEnv :: TypeEnv -> TypeEnv -> TypeEnv
unionTypeEnv (TypeEnv env) (TypeEnv env') = TypeEnv $ env `Map.union` env'

emptyEnv :: TypeEnv
emptyEnv = TypeEnv Map.empty

type InferM = ReaderT EnumInfo (State InferState)

runInferM
  :: EnumInfo
  -> InferState
  -> InferM a
  -> Either [TypeError] (a, [Constraint])
runInferM enumInfo initInferState inferM
  | null errs = Right (a, constrs)
  | otherwise = Left errs
  where
    (a, InferState _ errs constrs _) = runInferM' enumInfo initInferState inferM

runInferM'
  :: EnumInfo -> InferState -> InferM a -> (a, InferState)
runInferM' enumInfo inferState act
  = runState (runReaderT act enumInfo) inferState

-------------------------------------------------------------------------------
-- Preprocess Definitions (LAddress substitution)
-------------------------------------------------------------------------------

-- | Substitute LAddress literals for their respective literals
--   depending on the type of the value being defined
defnAddrSubst :: [Def] -> [Def]
defnAddrSubst = map substDefn
  where
    substDefn :: Def -> Def
    substDefn (GlobalDefNull typ nm) = GlobalDefNull typ nm
    substDefn (LocalDefNull typ nm) = LocalDefNull typ nm
    substDefn (GlobalDef typ nm llit) = GlobalDef typ nm $ substAddrLLit typ llit
    substDefn (LocalDef typ nm llit)  = LocalDef typ nm $ substAddrLLit typ llit

    substAddrLLit :: Type -> LLit -> LLit
    substAddrLLit t (Located loc lit) = Located loc $ substAddrLit t lit

substAddrLit :: Type -> Lit -> Lit
substAddrLit TAccount  (LAddress addr) = LAccount addr
substAddrLit (TAsset _) (LAddress addr) = LAsset addr
substAddrLit TContract (LAddress addr) = LContract addr
substAddrLit _         lit             = lit

-------------------------------------------------------------------------------
-- Type Signatures for Methods
-------------------------------------------------------------------------------

-- | Typechecks whether the values supplied as arguments to the method
-- call match the method argument types expected
tcMethod :: EnumInfo -> Method -> [Value] -> Either TypeErrInfo ()
tcMethod enumInfo method argVals
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

lookupSig :: Name -> [(Name,Sig)] -> Maybe Sig
lookupSig = lookup

signatures :: Script -> Either [TypeError] [(Name,Sig)]
signatures (Script enums defns graph methods)
  | null allErrs = Right sigs
  | otherwise = Left allErrs
  where
    enumInfo = createEnumInfo enums
    substDefns = defnAddrSubst defns
    defnInferState = snd $ runInferM' enumInfo initInferState $ tcDefns substDefns

    (methodErrs,sigs) = first concat $ partitionEithers $
      map (methodSig enumInfo defnInferState) methods

    allErrs = errs defnInferState ++ methodErrs

methodSig :: EnumInfo -> InferState -> Method -> Either [TypeError] (Name, Sig)
methodSig enumInfo defnInferState m@(Method tag methNm args body) = do
  checkEnumArgs args

  case runSolverM (errs inferState) (constrs inferState) of
    Left errs   -> Left errs
    Right subst -> Right $ (methNm,) $ Sig (argtys' m) $ apply subst retType
  where
    checkEnumArgs :: [Arg] -> Either [TypeError] ()
    checkEnumArgs args
      = case mapMaybe checkType args of
          [] -> Right ()
          errs@(_:_) -> Left errs
      where
      checkType (Arg (TEnum e) n)
        = if e `elem` Map.keys (enumToConstrs enumInfo)
          then Nothing
          else Just $ TypeError (UnknownEnum e) (located n)
      checkType _
        = Nothing

    (TypeInfo retType _ _, inferState) = runInferM' enumInfo methodInferState $ tcLExpr body

    methodInferState = InferState
      { count   = count defnInferState
      , errs    = [] -- reset so defn errs aren't reported multiple times
      , constrs = constrs defnInferState
      , env     = unionTypeEnv methodEnv (env defnInferState)
      }

    methodEnv :: TypeEnv
    methodEnv = foldl' extendEnv' emptyEnv args

    extendEnv' :: TypeEnv -> Arg -> TypeEnv
    extendEnv' env' (Arg typ (Located loc nm)) = extendEnv env' (nm, Temp, typeInfo)
      where
        typeInfo = TypeInfo
          { ttype = TRef typ
          , torig = ArgToMethod methNm
          , tloc  = loc
          }

-------------------------------------------------------------------------------
-- Typechecker (w/ inference)
-------------------------------------------------------------------------------

tcDefn :: Def -> InferM ()
tcDefn def = extendEnvM =<< case def of
  -- GlobalDef variable can be any type
  GlobalDefNull typ lnm -> do
    let Located loc nm = lnm
        typeInfo = TypeInfo (TRef typ) (VariableDefn nm) loc
    return (nm, Global, typeInfo)

  -- LocalDefNull variable can be any type
  LocalDefNull typ lnm -> do
    let Located loc nm = lnm
        typeInfo = TypeInfo (TRef typ) (VariableDefn nm) loc
    return (nm, Local, typeInfo)

  GlobalDef typ nm llit -> do
    (TypeInfo litType _ loc) <- tcLLit llit
    when (not $ validLitType typ litType) $ void $
      throwErrInferM (InvalidDefinition nm typ $ locVal llit) loc
    let typeInfo = TypeInfo (TRef typ) (VariableDefn nm) loc
    return (nm, Global, typeInfo)

  -- LocalDef variable can only be (TCrypto) TInt for right now
  LocalDef TInt nm llit -> do
    (TypeInfo litType _ loc) <- tcLLit llit
    when (not $ validLitType TInt litType) $ void $
      throwErrInferM (InvalidDefinition nm TInt $ locVal llit) loc
    let typeInfo = TypeInfo (TRef $ TCrypto TInt) (VariableDefn nm) loc
    return (nm, Local, typeInfo)

  -- Otherwise, LocalDef is invalid
  LocalDef typ nm (Located loc lit) -> do
    throwErrInferM (InvalidDefinition nm TInt lit) loc
    let typeInfo = TypeInfo TInt (VariableDefn nm) loc
    return (nm, Local, typeInfo)
  where
    validLitType :: Type -> Type -> Bool
    validLitType (TAsset _) TAssetAny = True
    validLitType TAssetAny (TAsset _) = True
    validLitType t1 t2 = t1 == t2

tcDefns :: [Def] -> InferM ()
tcDefns = mapM_ tcDefn

tcLExpr :: LExpr -> InferM TypeInfo
tcLExpr (Located loc expr) = case expr of
  ENoOp     -> return $ TypeInfo TVoid OutOfThinAir loc
  EVar nm   -> snd <$> lookupVarType' nm
  ELit llit -> tcLLit llit
  ESeq e e' -> tcLExpr e >> tcLExpr e'
  EBinOp nm e1 e2 -> tcBinOp nm e1 e2
  EUnOp nm e -> tcUnOp nm e

  ECall nm es -> case lookupPrim nm of
    Nothing -> throwErrInferM (InvalidPrimOp nm) loc
    Just prim -> tcPrim loc prim (nm, es)

  EAssign nm e -> do
    eTypeInfo@(TypeInfo eType _ eLoc)  <- tcLExpr e
    mVarTypeInfo <- lookupVarType (Located loc nm)
    case mVarTypeInfo of
      Nothing -> do -- New temp variable, instantiate it
        let typeInfo = TypeInfo (TRef eType) (InferredFromExpr $ locVal e) eLoc
        extendEnvM (nm, Temp, typeInfo)
      Just (varMeta, varTypeInfo) ->
        -- if var is Local var, typecheck with specific rules
        case varMeta of
          Local     -> tcLocalVarAssign nm e
          otherwise -> addConstr varTypeInfo eTypeInfo
    return $ TypeInfo TVoid Assignment eLoc

  EBefore edt e -> do
    dtTypeInfo <- tcLExpr edt
    eTypeInfo  <- tcLExpr e
    addConstr dtTypeInfo $ tDatetimeInfo DateTimeGuardPred loc
    addConstr eTypeInfo $ TypeInfo TVoid DateTimeGuardBody loc
    return $ TypeInfo TVoid DateTimeGuardBody loc

  EAfter edt e -> do
    dtTypeInfo <- tcLExpr edt
    eTypeInfo  <- tcLExpr e
    addConstr dtTypeInfo $ tDatetimeInfo DateTimeGuardPred loc
    addConstr eTypeInfo $ TypeInfo TVoid DateTimeGuardBody loc
    return $ TypeInfo TVoid DateTimeGuardBody loc

  EBetween startDte endDte e -> do
    startTypeInfo <- tcLExpr startDte
    endTypeInfo <- tcLExpr endDte

    let dtInfo = tDatetimeInfo DateTimeGuardPred loc
    addConstr startTypeInfo dtInfo
    addConstr endTypeInfo dtInfo

    eTypeInfo  <- tcLExpr e
    addConstr eTypeInfo $ TypeInfo TVoid DateTimeGuardBody loc

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
    addConstr cTypeInfo cTypeInfo'
    addConstr e1TypeInfo e2TypeInfo
    let retTypeInfo = TypeInfo
          { ttype = ttype e1TypeInfo
          , torig = IfCondition
          , tloc  = loc
          }
    return retTypeInfo

  ECase scrut ms -> do
    dTypeInfo <- tcLExpr scrut

    tcCasePatterns (located scrut) dTypeInfo . map matchPat $ ms

    -- Associate every body with its type info
    let attachInfo (Match _ bodyExpr) = (bodyExpr,) <$> tcLExpr bodyExpr
    bodiesTypeInfos <- mapM attachInfo ms
    -- Add a constraint for every body to have the same type as the
    -- first body
    case bodiesTypeInfos of
      [] -> throwErrInferM EmptyMatches $ located scrut
      ((bodyExpr, tyInfo):eis) -> do
        mapM_ (addConstr tyInfo . snd) eis
        return TypeInfo
          { ttype = ttype tyInfo
          , torig = CaseBody (locVal bodyExpr)
          , tloc  = located bodyExpr
          }

tcCasePatterns
  :: Loc
  -> TypeInfo
  -> [LPattern]
  -> InferM ()
tcCasePatterns topLoc scrutInfo []
  = void
    . throwErrInferM EmptyMatches
    $ topLoc
tcCasePatterns topLoc scrutInfo ps@(_:_)
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
                   mapM_ (addConstr scrutInfo <=< patternInfo enumConstrs) ps

                 (misses, overlaps)
                   -> void $ throwErrInferM (PatternMatchError misses overlaps) topLoc
      _ -> void $ throwErrInferM (CaseOnNotEnum scrutInfo) topLoc
  where
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
    LAccount addr  -> tcAddr addr TAccount
    LAsset addr    -> tcAddr addr TAssetAny
    LContract addr -> tcAddr addr TContract
    LState label   -> Right TState
    LAddress addr  -> Left (Impossible "Address literals should not happen.")
    LUndefined     -> Left (Impossible "Undefiend literals should not happen.")
    LDateTime _    -> Right TDateTime
    LTimeDelta _   -> Right TTimeDelta
    LConstr c  -> case Map.lookup c enumConstrs of
                    Nothing -> Left (Impossible "Reference to unknown enum constructor")
                    Just enum -> Right (TEnum enum)
  where
    tcAddr addr typ
      | validateAddress addr = Right typ
      | otherwise = Left $ InvalidAddress addr

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

tcPrim :: Loc -> PrimOp -> (Name, [LExpr]) -> InferM TypeInfo
tcPrim loc prim (nm,argExprs) = do

    -- Setup some prim op agnostic environment
    -- 1) Lookup what the type of the arguments of the prim op should be
    -- 2) Typecheck the arg exprs supplied to the prim op call
    -- 3) Create the type infos of the arg types
    (Sig argTypes retType) <- primSig prim
    arityCheck (Located loc nm) argTypes argExprs
    argExprTypeInfos <- mapM tcLExpr argExprs
    let argTypeOrig = PrimOpArg nm
        mkArgTypeInfo t lexpr = TypeInfo t argTypeOrig $ located lexpr
        argTypeInfos = zipWith mkArgTypeInfo argTypes argExprs

    -- Ok now typecheck
    case prim of

      -- AssetPrimOps must be typechecked uniquely-- Sometimes the type of one of
      -- the arguments to the prim op is dependent on the type of the asset
      -- supplied, and other times the _return type_ is dependent on the type of
      -- the asset supplied  as an argument.
      AssetPrimOp assetPrimOp ->
        case assetPrimOp of

          HolderBalance    -> do
            let [tassetAnyInfo,taccInfo] = argTypeInfos
                [tassetInfo,taccInfo']   = argExprTypeInfos
            -- add constraint for 1st arg to be an asset type
            addConstr tassetInfo tassetAnyInfo
            -- add constraint for 2nd arg to be an account type
            addConstr taccInfo taccInfo'
            case tassetInfo of
              TypeInfo (TAsset ta) torig tloc -> do
                -- We construct the type of the value that this prim op returns
                -- and then constrain the type variable return type to be this type
                let retTypeInfo = TypeInfo
                      (holdingsType ta)
                      (InferredFromAssetType nm (TAsset ta))
                      tloc
                addConstr retTypeInfo (TypeInfo retType (PrimOpRet nm) loc)
              TypeInfo t torig tloc -> do
                -- If the type is not an asset type, throw a type error
                let typeError = InvalidArgType nm t (ttype tassetAnyInfo)
                void $ throwErrInferM typeError tloc

          TransferHoldings -> do
            let [tacc1Info, tassetAnyInfo, tvarInfo, tacc2Info] = argTypeInfos
                [tacc1Info', tassetInfo, tbalInfo, tacc2Info']  = argExprTypeInfos
            -- add constraint for 1st arg to be an account
            addConstr tacc1Info tacc1Info'
            -- add constraint for 2nd arg to be an asset
            addConstr tassetAnyInfo tassetInfo
            -- * add constraint for 3rd arg depending on asset type
            tcHoldingsType (tassetInfo,tassetAnyInfo) (tbalInfo,tvarInfo)
            -- add constraint for 4th arg to be an account
            addConstr tacc2Info tacc2Info'

          TransferTo       -> do
            let [tassetAnyInfo, tvarInfo] = argTypeInfos
                [tassetInfo, tbalInfo]    = argExprTypeInfos
            -- add constraint for 1st arg to be an asset
            addConstr tassetAnyInfo tassetInfo
            -- * add constraint for 2nd arg depending on asset type
            tcHoldingsType (tassetInfo,tassetAnyInfo) (tbalInfo,tvarInfo)

          TransferFrom     -> do
            let [tassetAnyInfo, tvarInfo, taccInfo] = argTypeInfos
                [tassetInfo, tbalInfo, taccInfo']   = argExprTypeInfos
            -- add constraint for 1st arg to be an asset
            addConstr tassetAnyInfo tassetInfo
            -- * add constraint for 2nd arg depending on asset type
            tcHoldingsType (tassetInfo,tassetAnyInfo) (tbalInfo, tvarInfo)
            -- add constraint for 3rd arg to be an account
            addConstr taccInfo taccInfo'

          CirculateSupply -> do
            let [tassetAnyInfo, tvarInfo] = argTypeInfos
                [tassetInfo, tbalInfo]    = argExprTypeInfos
            -- add constraint for 1st arg to be an asset
            addConstr tassetAnyInfo tassetInfo
            -- * add constraint for 2nd arg depending on asset type
            tcHoldingsType (tassetInfo,tassetAnyInfo) (tbalInfo, tvarInfo)

      -- Normal primops are typechecked simply-- The expressions supplied as
      -- arguments must unify with the types of the arguments denoted in the prim
      -- op signature.
      normalPrimOp -> zipWithM_ addConstr argTypeInfos argExprTypeInfos

    -- The return type of all prim ops has either been contrained in the above
    -- code, or is a monomorphic type that is what it is, and should be the
    -- return type of the prim op as dictated by `primSig`
    return $ TypeInfo retType (PrimOpRet nm) loc

  where
    holdingsType :: TAsset -> Type
    holdingsType TDiscrete       = TInt
    holdingsType TBinary         = TBool
    holdingsType (TFractional p) = TFixed p

    -- Decide on how to constrain the tholdingsInfo argument depending on the
    -- type of the tassetInfo argument (the asset type) supplied to this prim op.
    tcHoldingsType :: (TypeInfo, TypeInfo) -> (TypeInfo, TypeInfo) -> InferM ()
    tcHoldingsType (tassetInfo,tassetAnyInfo) (tholdingsInfo,tvarInfo) =
      case tassetInfo of
        TypeInfo (TAsset ta) torig tloc ->
          -- If the asset arg is actually an asset type, constrain the type of
          -- the balance arg depending on it's holdings type.
          addConstr tholdingsInfo $ tvarInfo
            { ttype = holdingsType ta, torig = InferredFromAssetType nm (TAsset ta) }
        TypeInfo t torig tloc -> do
          -- If the type is not an asset type, throw a type error
          let typeError = InvalidArgType nm t (ttype tassetAnyInfo)
          void $ throwErrInferM typeError tloc

-- | Type signatures of builtin primitive operations.
primSig :: PrimOp -> InferM Sig
primSig = \case
  Verify              -> pure $ Sig [TAccount, TMsg, TSig] TBool
  Sign                -> pure $ Sig [TMsg] TSig
  Block               -> pure $ Sig [] TInt
  Deployer            -> pure $ Sig [] TAccount
  Sender              -> pure $ Sig [] TAccount
  Bound               -> pure $ Sig [TAssetAny, TAccount] TBool
  Created             -> pure $ Sig [] TInt
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

assetPrimSig :: AssetPrimOp -> InferM Sig
assetPrimSig = \case
  HolderBalance    -> Sig [TAssetAny, TAccount] <$> freshTAVar
  TransferHoldings -> freshTAVar >>= \tav -> pure $ Sig [TAccount, TAssetAny, tav, TAccount] TVoid -- from Account to Account
  TransferTo       -> freshTAVar >>= \tav -> pure (Sig [TAssetAny, tav] TVoid)                     -- from Account to Contract
  TransferFrom     -> freshTAVar >>= \tav -> pure (Sig [TAssetAny, tav, TAccount] TVoid)           -- from Contract to Account
  CirculateSupply  -> freshTAVar >>= \tav -> pure (Sig [TAssetAny, tav] TVoid)                     -- from Asset Supply to Asset issuer's holdings

-------------------------------------------------------------------------------
-- Valid Binary Op logic
-------------------------------------------------------------------------------

tcUnOp :: LUnOp -> LExpr -> InferM TypeInfo
tcUnOp (Located opLoc op) e = do
  eTypeInfo <- tcLExpr e
  let tcFunc = case op of
        Not -> tcNotOp
  let eLoc = located e
  tcFunc (eLoc,opLoc) (UnaryOperator op) eTypeInfo

tcBinOp :: LBinOp -> LExpr -> LExpr -> InferM TypeInfo
tcBinOp (Located opLoc op) e1 e2 = do
  e1TypeInfo <- tcLExpr e1
  e2TypeInfo <- tcLExpr e2
  let tcFunc = case op of
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
  let eLoc = located e1
  tcFunc (eLoc,opLoc) (BinaryOperator op) e1TypeInfo e2TypeInfo

-- | Multiplication is only valid for:
--     (TCrypto TInt) * TInt
--     TInt   * (TCrypto TInt)
--     TInt   * TInt
--     TFloat * TFloat
--     TDelta * TInt -- XXX
tcMult :: (Loc,Loc) -> TypeOrigin -> TypeInfo -> TypeInfo -> InferM TypeInfo
tcMult (eLoc,opLoc) torig tinfo1 tinfo2 =
  case (ttype tinfo1, ttype tinfo2) of
    (TTimeDelta, _)   -> addConstrAndRetInfo (tDeltaInfo torig eLoc) (tIntInfo torig opLoc, tinfo2)
    (_, TTimeDelta)   -> addConstrAndRetInfo (tDeltaInfo torig eLoc) (tinfo1, tIntInfo torig opLoc)
    (TCrypto TInt, _) -> addConstrAndRetInfo (tCryptoInfo torig eLoc) (tIntInfo torig opLoc, tinfo2)
    (_, TCrypto TInt) -> addConstrAndRetInfo (tCryptoInfo torig eLoc) (tinfo1, tIntInfo torig opLoc)
    (TInt, _)         -> addConstrAndRetInfo (tIntInfo torig eLoc) (tIntInfo torig opLoc, tinfo2)
    (_, TInt)         -> addConstrAndRetInfo (tIntInfo torig eLoc) (tinfo1, tIntInfo torig opLoc)
    (TFloat, _)       -> addConstrAndRetInfo (tFloatInfo torig eLoc) (tFloatInfo torig opLoc, tinfo2)
    (_, TFloat)       -> addConstrAndRetInfo (tFloatInfo torig eLoc) (tinfo1, tFloatInfo torig opLoc)
    (TFixed p, _)     -> addConstrAndRetInfo (tFixedInfo p torig eLoc) (tFixedInfo p torig opLoc, tinfo2)
    (_, TFixed p)     -> addConstrAndRetInfo (tFixedInfo p torig eLoc) (tinfo1, tFixedInfo p torig opLoc)
    (TVar a, TVar b)  -> do
      tinfo' <- TypeInfo <$> freshTVar <*> pure torig <*> pure eLoc
      addConstrAndRetInfo tinfo' (tinfo1, TypeInfo (TVar b) torig opLoc)
    (t1,t2)           -> do
      throwErrInferM (InvalidBinOp Mul t1 t2) opLoc
      return $ TypeInfo TError torig eLoc

-- | Add, Sub is only valid for:
--     (TCrypto TInt) +/- (TCrypto TInt)
--     TInt           +/- TInt
--     TFloat         +/- Float
--     TFixed         +/- TFixed
--     TDatetime      +/- TDelta
--     TDelta          +  TDelta
--     TMsg            +  TMsg (concatenation)
tcAddSub :: BinOp -> (Loc,Loc) -> TypeOrigin -> TypeInfo -> TypeInfo -> InferM TypeInfo
tcAddSub op (eLoc,opLoc) torig tinfo1 tinfo2 =
  case (ttype tinfo1, ttype tinfo2) of

    -- (TCrypto TInt) +/- (TCrypto TInt)
    (TCrypto TInt, _) -> addConstrAndRetInfo (tCryptoInfo torig eLoc) (tCryptoInfo torig opLoc, tinfo2)
    (_, TCrypto TInt) -> addConstrAndRetInfo (tCryptoInfo torig eLoc) (tinfo1, tCryptoInfo torig opLoc)

    -- TInt +/- TInt
    (TInt, _)         -> addConstrAndRetInfo (tIntInfo torig eLoc) (tIntInfo torig opLoc, tinfo2)
    (_, TInt)         -> addConstrAndRetInfo (tIntInfo torig eLoc) (tinfo1, tIntInfo torig opLoc)

    -- TFloat +/- Float
    (TFloat, _)       -> addConstrAndRetInfo (tFloatInfo torig eLoc) (tFloatInfo torig opLoc, tinfo2)
    (_, TFloat)       -> addConstrAndRetInfo (tFloatInfo torig eLoc) (tinfo1, tFloatInfo torig opLoc)

    -- TFixed +/- TFixed
    (TFixed p, _)     -> addConstrAndRetInfo (tFixedInfo p torig eLoc) (tFixedInfo p torig opLoc, tinfo2)
    (_, TFixed p)     -> addConstrAndRetInfo (tFixedInfo p torig eLoc) (tinfo1, tFixedInfo p torig opLoc)

    -- TDatetime +/- TDelta
    (TDateTime, _)    -> addConstrAndRetInfo (tDatetimeInfo torig eLoc) (tDeltaInfo torig opLoc, tinfo2)
    (_, TDateTime)    -> addConstrAndRetInfo (tDatetimeInfo torig eLoc) (tinfo1, tDeltaInfo torig opLoc)

    -- TDelta + TDelta (no subtraction)
    (TTimeDelta, t2)
      | op == Sub     -> do
          throwErrInferM (InvalidBinOp op TTimeDelta t2) opLoc
          return $ TypeInfo TError torig eLoc
      | otherwise     ->
          addConstrAndRetInfo (tDeltaInfo torig eLoc) (tinfo1, tDeltaInfo torig opLoc)
    (t1, TTimeDelta)
      | op == Sub     -> do
          throwErrInferM (InvalidBinOp op TTimeDelta t1) opLoc
          return $ TypeInfo TError torig eLoc
      | otherwise     ->
          addConstrAndRetInfo (tDeltaInfo torig eLoc) (tDeltaInfo torig opLoc, tinfo2)

    -- TMsg +  TMsg (concatenation, no subtraction)
    (TMsg, t2)
      | op == Sub     -> do
          throwErrInferM (InvalidBinOp op TMsg t2) opLoc
          return $ TypeInfo TError torig eLoc
      | otherwise     ->
          addConstrAndRetInfo (tMsgInfo torig eLoc) (tMsgInfo torig opLoc, tinfo2)
    (t1, TMsg)
      | op == Sub     -> do
          throwErrInferM (InvalidBinOp op t1 TMsg) opLoc
          return $ TypeInfo TError torig eLoc
      | otherwise     ->
          addConstrAndRetInfo (tMsgInfo torig eLoc) (tinfo1, tMsgInfo torig opLoc)

    (TVar a, TVar b)  -> do
      tinfo' <- TypeInfo <$> freshTVar <*> pure torig <*> pure eLoc
      addConstrAndRetInfo tinfo' (tinfo1, TypeInfo (TVar b) torig opLoc)

    (t1, t2)          -> do
      throwErrInferM (InvalidBinOp op t1 t2) opLoc
      return $ TypeInfo TError torig eLoc

-- | Division is only valid for:
--     TInt   / TInt
--     TFloat / TFLoat
tcDiv :: (Loc,Loc) -> TypeOrigin -> TypeInfo -> TypeInfo -> InferM TypeInfo
tcDiv (eLoc,opLoc) torig tinfo1 tinfo2 =
  case (ttype tinfo1, ttype tinfo2) of
    (TInt, _)         -> addConstrAndRetInfo (tIntInfo torig eLoc) (tIntInfo torig opLoc, tinfo2)
    (_, TInt)         -> addConstrAndRetInfo (tIntInfo torig eLoc) (tinfo1, tIntInfo torig opLoc)
    (TFloat, _)       -> addConstrAndRetInfo (tFloatInfo torig eLoc) (tFloatInfo torig opLoc, tinfo2)
    (_, TFloat)       -> addConstrAndRetInfo (tFloatInfo torig eLoc) (tinfo1, tFloatInfo torig opLoc)
    (TFixed p, _)     -> addConstrAndRetInfo (tFixedInfo p torig eLoc) (tFixedInfo p torig opLoc, tinfo2)
    (_, TFixed p)     -> addConstrAndRetInfo (tFixedInfo p torig eLoc) (tinfo1, tFixedInfo p torig opLoc)
    (TVar a, TVar b)  -> do
      tinfo' <- TypeInfo <$> freshTVar <*> pure torig <*> pure eLoc
      addConstrAndRetInfo tinfo' (tinfo1, TypeInfo (TVar b) torig opLoc)
    (t1,t2)           -> do
      throwErrInferM (InvalidBinOp Div t1 t2) opLoc
      return $ TypeInfo TError torig eLoc

tcAndOr :: (Loc,Loc) -> TypeOrigin -> TypeInfo -> TypeInfo -> InferM TypeInfo
tcAndOr (eLoc,opLoc) torig tinfo1 tinfo2 = do
  let argTypeInfo = TypeInfo TBool torig opLoc
  addConstr tinfo1 argTypeInfo
  addConstr argTypeInfo tinfo2
  return $ TypeInfo TBool torig eLoc

tcEqual :: BinOp -> (Loc,Loc) -> TypeOrigin -> TypeInfo -> TypeInfo -> InferM TypeInfo
tcEqual op (eLoc,opLoc) torig tinfo1 tinfo2 =
  case (ttype tinfo1, ttype tinfo2) of
    (TContract, _) -> addConstrAndRetBool (tContractInfo torig opLoc, tinfo2)
    (_, TContract) -> addConstrAndRetBool (tinfo1, tContractInfo torig opLoc)
    (TInt, _)      -> addConstrAndRetBool (tIntInfo torig opLoc, tinfo2)
    (_, TInt)      -> addConstrAndRetBool (tinfo1, tIntInfo torig opLoc)
    (TFloat, _)    -> addConstrAndRetBool (tFloatInfo torig opLoc, tinfo2)
    (_, TFloat)    -> addConstrAndRetBool (tinfo1, tFloatInfo torig opLoc)
    (TFixed p, _)  -> addConstrAndRetBool (tFixedInfo p torig opLoc, tinfo2)
    (_, TFixed p)  -> addConstrAndRetBool (tinfo1, tFixedInfo p torig opLoc)
    (TAccount, _)  -> addConstrAndRetBool (tAccountInfo torig opLoc, tinfo2)
    (_, TAccount)  -> addConstrAndRetBool (tinfo1, tAccountInfo torig opLoc)
    (TBool, _)     -> addConstrAndRetBool (tBoolInfo torig opLoc, tinfo2)
    (_, TBool)     -> addConstrAndRetBool (tinfo1, tBoolInfo torig opLoc)
    (TAsset at, _) -> addConstrAndRetBool (tAssetInfo at torig opLoc, tinfo2)
    (_, TAsset at) -> addConstrAndRetBool (tinfo1, tAssetInfo at torig opLoc)
    (TDateTime, _) -> addConstrAndRetBool (tDatetimeInfo torig opLoc, tinfo2)
    (_, TDateTime) -> addConstrAndRetBool (tinfo1, tDatetimeInfo torig opLoc)
    (TTimeDelta, _) -> addConstrAndRetBool (tDeltaInfo torig opLoc, tinfo2)
    (_, TTimeDelta) -> addConstrAndRetBool (tinfo1, tDeltaInfo torig opLoc)
    (TMsg, _)      -> addConstrAndRetBool (tMsgInfo torig opLoc, tinfo2)
    (_, TMsg)      -> addConstrAndRetBool (tinfo1, tMsgInfo torig opLoc)
    (TVar a, TVar b) -> addConstrAndRetBool (tinfo1, TypeInfo (TVar b) torig opLoc)
    (t1,t2)        -> do
      throwErrInferM (InvalidBinOp op t1 t2) opLoc
      return $ tBoolInfo torig eLoc
  where
    addConstrAndRetBool = addConstrAndRetInfo $ tBoolInfo torig eLoc

tcLEqual :: BinOp -> (Loc,Loc) -> TypeOrigin -> TypeInfo -> TypeInfo -> InferM TypeInfo
tcLEqual op (eLoc,opLoc) torig tinfo1 tinfo2 =
  case (ttype tinfo1, ttype tinfo2) of
    (TInt, _)        -> addConstrAndRetBool (tIntInfo torig opLoc, tinfo2)
    (_, TInt)        -> addConstrAndRetBool (tinfo1, tIntInfo torig opLoc)
    (TFloat, _)      -> addConstrAndRetBool (tFloatInfo torig opLoc, tinfo2)
    (_, TFloat)      -> addConstrAndRetBool (tinfo1, tFloatInfo torig opLoc)
    (TFixed p, _)    -> addConstrAndRetBool (tFixedInfo p torig opLoc, tinfo2)
    (_, TFixed p)    -> addConstrAndRetBool (tinfo1, tFixedInfo p torig opLoc)
    (TDateTime, _)   -> addConstrAndRetBool (tDatetimeInfo torig opLoc, tinfo2)
    (_, TDateTime)   -> addConstrAndRetBool (tinfo1, tDatetimeInfo torig opLoc)
    (TTimeDelta, _)  -> addConstrAndRetBool (tDeltaInfo torig opLoc, tinfo2)
    (_, TTimeDelta)  -> addConstrAndRetBool (tinfo1, tDeltaInfo torig opLoc)
    (TVar a, TVar b) -> addConstrAndRetBool (tinfo1, TypeInfo (TVar b) torig opLoc)
    (t1,t2)          -> do
      throwErrInferM (InvalidBinOp op t1 t2) opLoc
      return $ TypeInfo TError torig eLoc
  where
    addConstrAndRetBool = addConstrAndRetInfo $ tBoolInfo torig eLoc

tcGEqual :: (Loc,Loc) -> TypeOrigin -> TypeInfo -> TypeInfo -> InferM TypeInfo
tcGEqual = tcLEqual GEqual

tcGreater :: (Loc,Loc) -> TypeOrigin -> TypeInfo -> TypeInfo -> InferM TypeInfo
tcGreater = tcLEqual Greater

tcLesser :: (Loc,Loc) -> TypeOrigin -> TypeInfo -> TypeInfo -> InferM TypeInfo
tcLesser = tcLEqual Lesser

tcNEqual :: (Loc,Loc) -> TypeOrigin -> TypeInfo -> TypeInfo -> InferM TypeInfo
tcNEqual = tcEqual NEqual

tcNotOp :: (Loc,Loc) -> TypeOrigin -> TypeInfo -> InferM TypeInfo
tcNotOp (eLoc,opLoc) torig tinfo =
  case ttype tinfo of
    TBool       -> return $ tBoolInfo torig eLoc
    invalidUnOp -> do
      throwErrInferM (InvalidUnOp Not invalidUnOp) opLoc
      return $ TypeInfo TError torig eLoc

-- | Helper for common pattern "Add constraint of two TypeInfos and return a TypeInfo"
addConstrAndRetInfo :: TypeInfo -> (TypeInfo, TypeInfo) -> InferM TypeInfo
addConstrAndRetInfo retInfo (tinfo1, tinfo2) =
  addConstr tinfo1 tinfo2 >> return retInfo

-------------------------------------------------------------------------------
-- Special Typechecking for Assignment of Local Vars -- XXX
-------------------------------------------------------------------------------

-- | Currently, the only valid local variable assignment exprs are:
--
--     <local1> = <local2>
--     <local1> = <local1>   <BINOP>  <local2>
--     <local1> = <local2>   <BINOP>  <local1>
--     <local1> = <local1>   <BINOP>  <global1>
--     <local1> = <global1>  <BINOP>  <local1>
--     <local1> = <local1>   <BINOP>  <literal>
--     <local1> = <literal>  <BINOP>  <local1>
--
-- This will be cleaned up in the future, where we support more expressive deltas
-- to emit during the evaluation process in Eval.hs. This is an ad-hoc, temporary
-- fix. At some point, we wish to allow local variables to be assigned to
-- arbitrarily deeply nested BinOp exprs as long as at least a single operand in
-- the binop expr is the local variable on the LHS of the assignment.

tcLocalVarAssign :: Name -> LExpr -> InferM ()
tcLocalVarAssign localVarNm lexpr@(Located eloc expr) = do

  lhsTypeInfo <- snd <$> lookupVarType' (Located eloc localVarNm)
  errTypeInfo <- tcLExpr lexpr

  case expr of

    -- If expr on RHS is a local EVar, type is valid
    EVar var -> do
      (rhsMeta, rhsTypeInfo) <- lookupVarType' var
      case rhsMeta of
        Local -> addConstr lhsTypeInfo rhsTypeInfo
        _ -> void $ flip throwErrInferM eloc $
          flip InvalidLocalVarAssign rhsTypeInfo
            "Variable on the RHS of Local var assignment must be another local var"

    -- If x <op> y follows on of the patterns described above, type is valid
    EBinOp op x y -> do

      case (locVal x, locVal y) of

        (EVar var1, EVar var2) -> do
          var1TypeInfo@(TypeInfo v1type v1orig v1Loc) <- snd <$> lookupVarType' var1
          var2TypeInfo@(TypeInfo v2type v2orig v2Loc) <- snd <$> lookupVarType' var2
          -- One of the variables must be the same var as in the LHS of the assignment
          if | locVal var1 == localVarNm -> void $ tcBinOp op x y
             | locVal var2 == localVarNm -> void $ tcBinOp op x y
             | otherwise -> void $ flip throwErrInferM eloc $
                 flip InvalidLocalVarAssign errTypeInfo $
                   "One of the variables in the EBinOp expr must be " <> show localVarNm

        (EVar var, ELit llit)
          -- the var must be the same as on the LHS of the assignment
          | locVal var == localVarNm -> do
              litTypeInfo <- tcLLit llit
              addConstr lhsTypeInfo litTypeInfo
          | otherwise -> void $ flip throwErrInferM eloc $
              flip InvalidLocalVarAssign errTypeInfo $
                "The variable in the EBinOp expr must be " <> show localVarNm

        (ELit llit, EVar var)
          -- the var must be the same as on the LHS of the assignment
          | locVal var == localVarNm -> do
              litTypeInfo <- tcLLit llit
              addConstr lhsTypeInfo litTypeInfo
          | otherwise -> void $ flip throwErrInferM eloc $
              flip InvalidLocalVarAssign errTypeInfo $
                "The variable in the EBinOp expr must be " <> show localVarNm

        (x',y') -> void $ flip throwErrInferM eloc $
          flip InvalidLocalVarAssign errTypeInfo $
            "A BinOp on the RHS of a local var assignment must be of a particular form"

    otherwise -> void $ flip throwErrInferM eloc $
      flip InvalidLocalVarAssign errTypeInfo $
        "A Local var assignment must be to either another local var, or a bin op"
-------------------------------------------------------------------------------
-- Contraint Generation
-------------------------------------------------------------------------------

data Constraint = Constraint TypeInfo TypeInfo
  deriving (Show)

addConstr :: TypeInfo -> TypeInfo -> InferM ()
addConstr tinfo1 tinfo2 = modify' $ \s ->
  s { constrs = constrs s ++ [constr] }
  where
    constr = Constraint tinfo1 tinfo2

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

lookupVarType :: LName -> InferM (Maybe (TMeta, TypeInfo))
lookupVarType (Located loc name) = do
  (TypeEnv typeEnv) <- env <$> get
  return $ second deRefInfo <$> Map.lookup name typeEnv

lookupVarType' :: LName -> InferM (TMeta, TypeInfo)
lookupVarType' var@(Located loc name) = do
  mVarTypeInfo <- lookupVarType var
  case mVarTypeInfo of
    Nothing -> (Temp,) <$> throwErrInferM (UnboundVariable name) loc
    Just typeInfo -> return $ second deRefInfo typeInfo

-- | Checks if # args suppltiied to function match # args in Sig
arityCheck :: LName -> [Type] -> [LExpr] -> InferM ()
arityCheck (Located loc nm) typs args
  | lenTyps == lenArgs = return ()
  | otherwise = void $ throwErrInferM (ArityFail nm lenTyps lenArgs) loc
  where
    lenTyps = length typs
    lenArgs = length args

deRef :: Type -> Type
deRef (TRef t) = t
deRef t = t

deRefInfo :: TypeInfo -> TypeInfo
deRefInfo tinfo = tinfo { ttype = deRef (ttype tinfo) }

-------------------------------------------------------------------------------
-- Substitution
-------------------------------------------------------------------------------

class Substitutable a where
  apply :: Subst -> a -> a

instance Substitutable Type where
  apply s t@(TVar a)  = Map.findWithDefault t a s
  apply s (TRef t)    = TRef (apply s t)
  apply s (TCrypto t) = TCrypto (apply s t)
  apply s TError      = TError
  apply s TInt        = TInt
  apply s TFloat      = TFloat
  apply s (TFixed p)  = TFixed p
  apply s TBool       = TBool
  apply s TAccount    = TAccount
  apply s (TAsset at) = TAsset at
  apply s TContract   = TContract
  apply s TAddress    = TAddress
  apply s TMsg        = TMsg
  apply s TVoid       = TVoid
  apply s TSig        = TSig
  apply s TAny        = TAny
  apply s TAssetAny   = TAssetAny
  apply s TState      = TState
  apply s TDateTime   = TDateTime
  apply s TTimeDelta  = TTimeDelta
  apply s t@TEnum{}   = t

instance (Substitutable a, Substitutable b) => Substitutable (a,b) where
  apply s (a,b) = (apply s a, apply s b)

instance Substitutable TypeInfo where
  apply s (TypeInfo t info tPos) = TypeInfo (apply s t) info tPos

instance Substitutable TMeta where
  apply s tmeta = tmeta

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply

instance Substitutable Constraint where
  apply s (Constraint t1 t2) = Constraint (apply s t1) (apply s t2)

instance Substitutable TypeEnv where
  apply s (TypeEnv env) =  TypeEnv $ Map.map (apply s) env

type Subst = Map.Map TVar Type

emptySubst :: Subst
emptySubst = Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

-------------------------------------------------------------------------------
-- Unification & Solving
-------------------------------------------------------------------------------

type Unifier = (Subst, [Constraint])

emptyUnifier :: Unifier
emptyUnifier = (emptySubst, [])

type SolverM = State [TypeError]

runSolverM :: [TypeError] -> [Constraint] -> Either [TypeError] Subst
runSolverM typeErrs constrs
  | null errs = Right subst
  | otherwise = Left errs
  where
    (subst, errs) = flip runState typeErrs $ solver (emptySubst,constrs)

throwErrSolverM :: TypeError -> SolverM Unifier
throwErrSolverM typeErr = do
  modify' $ flip (++) [typeErr]
  return emptyUnifier

bind ::  TVar -> Type -> Unifier
bind tv t = (Map.singleton tv t, [])

unify :: TypeInfo -> TypeInfo -> SolverM Unifier
unify to1@(TypeInfo t1 _ t1Pos) to2@(TypeInfo t2 _ t2Pos) =
    case unify' t1 t2 of
      Left tErrInfo -> throwErrSolverM $ TypeError tErrInfo $ max t1Pos t2Pos
      Right unifier -> return unifier
  where
    unify' :: Type -> Type -> Either TypeErrInfo Unifier
    unify' t1 t2 | t1 == t2 = Right emptyUnifier
    unify' TAny   _     = Right emptyUnifier
    unify' _      TAny  = Right emptyUnifier
    unify' (TVar v) t   = unifyTVar t v
    unify' t (TVar v)   = unifyTVar t v
    unify' TAssetAny (TAsset ta) = Right emptyUnifier
    unify' (TAsset ta) TAssetAny = Right emptyUnifier
    unify' TError t     = Right emptyUnifier
    unify' t TError     = Right emptyUnifier
    unify' t1 t2        = Left $ UnificationFail to1 to2

    unifyTVar :: Type -> TVar -> Either TypeErrInfo Unifier
    -- TV (general type vars) unify with anything
    unifyTVar t            v@(TV _)  = Right $ v `bind` t
    -- TAV (asset specific type vars) unify with only 3 types
    unifyTVar TInt         v@(TAV _) = Right $ v `bind` TInt
    unifyTVar TBool        v@(TAV _) = Right $ v `bind` TBool
    unifyTVar t@(TFixed _) v@(TAV _) = Right $ v `bind` t
    unifyTVar _            v@(TAV _) = Left $ UnificationFail to1 to2

solver :: Unifier -> SolverM Subst
solver (su, cs) =
  case cs of
    [] -> return su
    ((Constraint t1 t2): cs) -> do
      (su', cs') <- unify t1 t2
      solver (su' `composeSubst` su, cs' ++ apply su' cs)

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
    BinaryOperator op   -> "was inferred from use of binary operator" <+> squotes (ppr op)
    UnaryOperator op    -> "was inferred from use of unary operator" <+> squotes (ppr op)
    Assignment          -> "was inferred from variable assignment"
    IfCondition         -> "must be a bool because of if statement"
    DateTimeGuardPred   -> "must be a datetime because it is a datetime guard predicate"
    DateTimeGuardBody   -> "must be a void because it is the body of a datetime guard"
    ArgToMethod nm      -> "was inferred because of the type signature of" <+> squotes (ppr nm)
    PrimOpArg nm        -> "was inferred from the argument type of prim op" <+> squotes (ppr nm)
    PrimOpRet nm        -> "was inferred from the return  type of prim op" <+> squotes (ppr nm)
    CasePattern nm      -> "was inferred from type of case pattern" <+> squotes (ppr nm)
    CaseBody e          -> "was inferred from type of case body" <+> squotes (ppr e)

instance Pretty TypeInfo where
  ppr (TypeInfo t orig loc) = "Type" <+> ppr t <+> ppr orig <+> "on" <+> ppr loc

instance Pretty TypeErrInfo where
  ppr e = case e of
    UnboundVariable nm            -> "Unbound variable: " <+> ppr nm
    InvalidDefinition nm typ lit  -> "Invalid definition for" <+> ppr nm <> ":"
                                  <$$+> "Literal " <> ppr lit <+> "does not have type" <+> ppr typ
    InvalidPrimOp nm              -> "Invalid primitive operation: " <+> ppr nm
    InvalidReturnType             -> "Invalid return type: "
                                  <$$+> "Local variables or expressions involving computations over local"
                                  <+> "variable cannot be returned from functions."
    InvalidBinOp op t1 t2         -> "Invalid binary operation: "
                                  <$$+> squotes (ppr op) <+> "does not accept types" <+> ppr t1 <+> "and" <+> ppr t2
    InvalidUnOp op t              -> "Invalid unary operation: "
                                  <$$+> squotes (ppr op) <+> "does not accept types" <+> ppr t
    InvalidAddress nm             -> "Invalid address: " <+> ppr nm
                                  <$$+> "Addresses must be a valid base 58 encoded sha256 hashes."
    ArityFail nm n m              -> "Arity mismatch in function call" <+> ppr nm <> ":"
                                  <$$+> "Expecting" <+> ppr n <+> "arguments, but got" <+> ppr m
    InvalidArgType nm t1 t2       -> "Invlalid argument type to method" <+> ppr nm <> ":"
                                  <$$+> "Expecting type" <+> ppr t1 <+> "but got" <+> ppr t2
    UnificationFail tinfo1 tinfo2 -> "Cannot unify type"
                                  <+> squotes (ppr $ ttype tinfo1)
                                  <+> "with type"
                                  <+> squotes (ppr $ ttype tinfo2) <> ":"
                                  <$$+> ppr tinfo1
                                  <$$+> ppr tinfo2
    InvalidLocalVarAssign t tinfo -> "Invalid local var assignment: "
                                  <$$+> ppr t
                                  <$$+> ppr tinfo
    CaseOnNotEnum e               -> "Case analysis on a non-enum type:"
                                  <$$+> ppr e
    UnknownConstructor c          -> "Reference to undefined constructor:"
                                  <$$+> ppr c
    UnknownEnum e                 -> "Reference to undefined enum type:"
                                  <$$+> ppr e
    PatternMatchError misses dups -> "Pattern match failures:"
                                  <$$+> vsep (map ((" - Missing case for: " <>) . ppr) misses)
                                  <$$+> vsep (map ((" - Duplicate case for: " <>) . ppr) dups)
    EmptyMatches                  -> "Case expression with no matches"
    Impossible msg                -> "The impossible happened: " <+> ppr msg

instance Pretty TypeError where
  ppr (TypeError tErrInfo tPos) = "Type error at" <+> ppr tPos <> ":"
                             <$$+> ppr tErrInfo

instance Pretty [TypeError] where
  ppr es = case map ppr (sort es) of
    [] -> ""
    (e:es) -> foldl' (<$$$>) (ppr e) $ map ppr es

-- | Pretty print a type error
ppError :: TypeError -> LText
ppError = render . ppr

-- | Pretty print a type signature
ppSig :: Sig -> LText
ppSig = render . ppr
