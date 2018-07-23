{-|

FCL interpreter and expression evaluation.

-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Script.Eval (
  -- ** Evaluation monad
  EvalM,
  EvalFail(..),
  runEvalM,
  execEvalM,

  -- ** Evaluation rules
  eval,
  evalLLit,
  evalLExpr,
  evalMethod,
  insertTempVar,

  -- ** Evaluation state
  EvalState(..),
  initEvalState,

  -- ** Evaluation context
  EvalCtx(..),
) where

import Protolude hiding (DivideByZero, Overflow, Underflow)

import Prelude (read)

import Fixed
import Script
import SafeInteger
import SafeString as SS
import Key (PrivateKey)
import Time (Timestamp, posixMicroSecsToDatetime)
import Ledger (World)
import Storage
import Contract (Contract, LocalStorageVars(..))
import Account (Account,  address, publicKey)
import Script.Prim (PrimOp(..))
import Script.Error as Error
import Script.Graph (GraphState(..), terminalLabel, initialLabel)
import Address (Address, AContract, AAccount, AAsset, rawAddr)
import Utils (panicImpossible)
import qualified Asset
import qualified Delta
import qualified Contract
import qualified Hash
import qualified Key
import qualified Ledger
import qualified Script.Prim as Prim

import qualified Datetime as DT
import Datetime.Types (within, Interval(..), add, sub, subDeltas, scaleDelta)
import qualified Datetime.Types as DT

import Data.Fixed (Fixed(..), showFixed)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Serialize as S

import qualified Control.Exception as E
import qualified Control.Monad.Catch as Catch
import Control.Monad.State
import qualified Crypto.Random as Crypto (SystemDRG, getSystemDRG)
import qualified Crypto.Random.Types as Crypto (MonadRandom(..), MonadPseudoRandom, withDRG)

import qualified Encoding

-------------------------------------------------------------------------------
-- Execution State
-------------------------------------------------------------------------------

-- | Evaluation context used during remote evaluation in a validating engine.
data EvalCtx = EvalCtx
  { currentBlock       :: Int64             -- ^ The latest block in the chain
  , currentValidator   :: Address AAccount   -- ^ Referencing an account
  , currentTransaction :: Hash.Hash Encoding.Base16ByteString   -- ^ Current transaction hash
  , currentTimestamp   :: Time.Timestamp    -- ^ When the current method is being executed
  , currentCreated     :: Time.Timestamp    -- ^ When the contract was deployed
  , currentDeployer    :: Address AAccount  -- ^ Referencing an account
  , currentTxIssuer    :: Address AAccount   -- ^ Transaction sender
  , currentAddress     :: Address AContract -- ^ Address of current Contract
  , currentPrivKey     :: Key.PrivateKey    -- ^ Private key of Validating node
  , currentHelpers     :: [Helper]          -- ^ Script helper functions available for call in method body
  } deriving (Generic, NFData)

type LocalStorages = Map (Address AAccount) Storage

data EvalState = EvalState
  { tempStorage      :: Storage          -- ^ Tmp variable env
  , globalStorage    :: Storage          -- ^ Global variable env
  , localStorageVars :: LocalStorageVars -- ^ Set of local var names
  , localStorage     :: LocalStorages    -- ^ Local Variable per counter party
  , graphState       :: GraphState       -- ^ Current state of contract
  , worldState       :: World            -- ^ Current world state
  , sideState        :: Maybe SideState  -- ^ Current state
  , sideLock         :: (Bool, Lock)     -- ^ Lock state
  , deltas           :: [Delta.Delta]
  } deriving (Generic, NFData)

data SideState = SideInit | SideStop
  deriving (Generic, NFData)

type Lock = Maybe (Time.Timestamp, Int64)

{- TopLevel:
 -
 - NodeData
 -   > latestBlock index
 -   > node account address
 -   > ledger state
 -   > private key
 -   > storage key
 -
 - Transaction
 -   > current transaction hash
 -   > origin address of transaction
 -
 - Contract
 -   > timestamp
 -   > address
 -}

initEvalState :: Contract -> World -> EvalState
initEvalState c w = EvalState
  { tempStorage      = mempty
  , globalStorage    = Storage.unGlobalStorage (Contract.globalStorage c)
  , localStorage     = map Storage.unLocalStorage (Contract.localStorage c)
  , localStorageVars = Contract.localStorageVars c
  , graphState       = Contract.state c
  , sideState        = Nothing
  , sideLock         = (False, Nothing)
  , worldState       = w
  , deltas           = []
  }

-------------------------------------------------------------------------------
-- Interpreter Steps
-------------------------------------------------------------------------------

lookupGlobalVar :: Name -> EvalM (Maybe Value)
lookupGlobalVar (Name var) = do
  globalStore <- gets globalStorage
  return $ Map.lookup (Key var) globalStore


isLocalVar :: Name -> (EvalM Bool)
isLocalVar name = do
  vars <- Contract.unLocalStorageVars <$> gets localStorageVars
  return $ name `Set.member` vars

lookupTempVar :: Name -> EvalM (Maybe Value)
lookupTempVar (Name var) = do
  tmpStore <- gets tempStorage
  return $ Map.lookup (Key var) tmpStore

insertTempVar :: Name -> Value -> EvalM ()
insertTempVar (Name var) val = modify' $ \evalState ->
    evalState { tempStorage = insertVar (tempStorage evalState) }
  where
    insertVar = Map.insert (Key var) val

-- | Extends the temp storage with temporary variable updates. Emulates a
-- closure environment for evaluating the body of helper functions by
-- assigning values to argument names. Effectively ad-hoc substitution.
localTempStorage :: [(Name,Value)] -> EvalM a -> EvalM a
localTempStorage varVals evalM = do
  currTempStorage <- tempStorage <$> get
  let store = Map.fromList (map (first (Key . unName)) varVals)
      newTempStorage = Map.union store currTempStorage
  modify $ \evalState ->
    evalState { tempStorage = newTempStorage }
  res <- evalM
  modify $ \evalState ->
    evalState { tempStorage = currTempStorage }
  pure res

-- | Warning: This function will throw an exception on a non-existent helper, as
-- this indicates the typechecker failed to spot an undefined function name.
lookupHelper :: LName -> EvalM Helper
lookupHelper lhnm = do
  helpers <- currentHelpers <$> ask
  case List.find ((==) lhnm . helperName) helpers of
    Nothing     -> panicImpossible $ Just "lookupHelper: Undefined helper function name"
    Just helper -> pure helper

-- | Emit a delta updating  the state of a global reference.
updateGlobalVar :: Name -> Value -> EvalM ()
updateGlobalVar (Name var) val = modify' $ \evalState ->
    evalState { globalStorage = updateVar (globalStorage evalState) }
  where
    updateVar = Map.update (\_ -> Just val) (Key var)

-- | Updating a local variable only happens if the evaluating node is storing
-- a local storage for an account involved in the contract and method being evaluated
updateLocalVar :: Name -> Value -> EvalM ()
updateLocalVar (Name var) val = modify' $ \evalState ->
    evalState { localStorage = map updateLocalVar' (localStorage evalState) }
  where
    updateLocalVar' :: Storage -> Storage
    updateLocalVar' = Map.insert (Key $ toS var) val

setWorld :: World -> EvalM ()
setWorld w = modify' $ \evalState -> evalState { worldState = w }

-- | Halt if the execution is terminate state.
guardTerminate :: EvalM ()
guardTerminate = do
  s <- gets graphState
  case s of
    GraphTerminal -> throwError TerminalState
    _             -> pure ()

-- | Update the evaluate state.
updateState :: GraphState -> EvalM ()
updateState state = modify $ \s -> s { graphState = state }

-- | Get the evaluation state
getState :: EvalM GraphState
getState = gets graphState

-- | Emit a delta
emitDelta :: Delta.Delta -> EvalM ()
emitDelta delta = modify' $ \s -> s { deltas = deltas s ++ [delta] }

-- | Lock main graph, switch to side graph
sideInit :: Int64 -> EvalM ()
sideInit timeout = do
  now <- currentTimestamp <$> ask
  modify' $ \s -> s { sideState = Just SideInit, sideLock = (True, Just (now, now+timeout)) }

-- | Unlock side graph, switch to main graph
sideUnlock :: EvalM ()
sideUnlock = do
  modify' $ \s -> s { sideState = Nothing, sideLock = (False, Nothing) }

sideStop :: EvalM ()
sideStop = modify' $ \s -> s { sideState = Just SideStop }

-- | Lookup variable in scope
lookupVar :: Name -> EvalM (Maybe Value)
lookupVar var = do
  gVar <- lookupGlobalVar var
  case gVar of
    Nothing  -> lookupTempVar var
    Just val -> return $ Just val

-------------------------------------------------------------------------------
-- Interpreter Monad
-------------------------------------------------------------------------------

type RandomM = Crypto.MonadPseudoRandom Crypto.SystemDRG

-- | Initialize the random number generator and run the monadic
-- action.
runRandom :: RandomM a -> IO a
runRandom m = do
  gen <- Crypto.getSystemDRG
  return . fst . Crypto.withDRG gen $ m

-- | EvalM monad
type EvalM = ReaderT EvalCtx (StateT EvalState (ExceptT Error.EvalFail RandomM))

instance Crypto.MonadRandom EvalM where
  getRandomBytes = lift . lift . lift . Crypto.getRandomBytes

-- | Run the evaluation monad.
execEvalM :: EvalCtx -> EvalState -> EvalM a -> IO (Either Error.EvalFail EvalState)
execEvalM evalCtx evalState
  = handleArithError
  . runRandom
  . runExceptT
  . flip execStateT evalState
  . flip runReaderT evalCtx

-- | Run the evaluation monad.
runEvalM :: EvalCtx -> EvalState -> EvalM a -> IO (Either Error.EvalFail (a, EvalState))
runEvalM evalCtx evalState
  = handleArithError
  . runRandom
  . runExceptT
  . flip runStateT evalState
  . flip runReaderT evalCtx

handleArithError :: IO (Either Error.EvalFail a) -> IO (Either Error.EvalFail a)
handleArithError m = do
   res <- Catch.try $! m
   case res of
    Left E.Overflow              -> return . Left $ Overflow
    Left E.Underflow             -> return . Left $ Underflow
    Left (e :: E.ArithException) -> return . Left $ Impossible "Arithmetic exception"
    Right val                    -> pure val

-------------------------------------------------------------------------------
-- Evaluation
-------------------------------------------------------------------------------

-- | Evaluator for expressions
evalLExpr :: LExpr -> EvalM Value
evalLExpr (Located _ e) = case e of

  ESeq a b        -> evalLExpr a >> evalLExpr b

  ELit llit       -> pure $ evalLLit llit

  EAssign lhs rhs -> do
    gVal <- lookupGlobalVar lhs
    case gVal of
      Nothing -> do
        isLhsLocal <- isLocalVar lhs
        if isLhsLocal
           then evalAssignLocal lhs rhs
           else do
             v <- evalLExpr rhs
             insertTempVar lhs v
      Just initVal -> do
        resVal <- evalLExpr rhs
        when (initVal /= resVal) $ do
          updateGlobalVar lhs resVal
          emitDelta $ Delta.ModifyGlobal lhs resVal

    pure VVoid

      where
        evalAssignLocal :: Name -> LExpr -> EvalM ()
        evalAssignLocal lhs rhs = do
          case locVal rhs of

            -- RHS is a local variable: emit Replace delta
            EVar lvar -> do
              isRhsLocal <- isLocalVar $ locVal lvar
              if isRhsLocal
                then do
                  evalAndUpdateLocal lhs rhs
                  emitDelta $ Delta.ModifyLocal lhs $ Delta.Replace (locVal lvar)
                else panicImpossible $ Just "Assigning a local var to a non local var"

            -- RHS is a BinOp with a local var at the top level: emit Op delta
            EBinOp locOp x y -> do
              let op = locVal locOp
              case (locVal x, locVal y) of

                (EVar var1, EVar var2)
                  -- If var1 is LHS local var, eval y and emit delta
                  | locVal var1 == lhs -> do
                      evalAndUpdateLocal lhs rhs
                      yVal <- evalLExpr y
                      emitDelta $ Delta.ModifyLocal (locVal var1) $ Delta.Op op yVal

                  -- If var2 is LHS local var, eval x and emit delta
                  | locVal var2 == lhs -> do
                      evalAndUpdateLocal lhs rhs
                      xVal <- evalLExpr x
                      emitDelta $ Delta.ModifyLocal (locVal var2) $ Delta.Op op xVal

                  | otherwise -> panicImpossible $
                      Just "evalAssignLocal: var1 or var2 must be local variables"

                (EVar var, e) -> do
                  let varName = locVal var
                  if varName == lhs
                    then do -- IFF var x is the local var on LHS, eval y and emit delta
                      evalAndUpdateLocal lhs rhs
                      yVal <- evalLExpr y
                      emitDelta $ Delta.ModifyLocal varName $ Delta.Op op yVal
                    else panicImpossible $
                      Just "evalAssignLocal: var must be local variable"

                (e, EVar var) -> do
                  let varName = locVal var
                  if varName == lhs
                    then do -- IFF var y is the local var on LHS, eval x and emit delta
                      evalAndUpdateLocal lhs rhs
                      xVal <- evalLExpr x
                      emitDelta $ Delta.ModifyLocal varName $ Delta.Op op xVal
                    else panicImpossible $
                      Just "evalAssignLocal: var must be local variable"

                _ -> panicImpossible $ Just "Local var binops have to have a local var on either the lhs or rhs"

            _ -> panicImpossible $ Just "Local var binops have to have a local var on either the lhs or rhs"

        -- If local var exists in memory (issuer or counterparty interacting
        -- with contract) then update it's value in memory
        evalAndUpdateLocal :: Name -> LExpr -> EvalM ()
        evalAndUpdateLocal localVar e =
          flip catchError (const $ pure ()) $ do
            eVal <- evalLExpr e
            updateLocalVar localVar eVal

  EUnOp (Located _ op) a -> do
    valA <- evalLExpr a
    let unOpFail = panicInvalidUnOp op valA
    case valA of
      VBool a' -> return $
        case op of
          Script.Not -> VBool $ not a'
      _ -> panicImpossible $ Just "EUnOp"

  -- This logic handles the special cases of operating over homomorphic
  -- crypto-text.
  EBinOp (Located _ op) a b -> do
    valA <- evalLExpr a
    valB <- evalLExpr b
    let binOpFail = panicInvalidBinOp op valA valB
    case (valA, valB) of
      (VInt a', VInt b') ->
        case op of
          Script.Add -> pure $ VInt (a' + b')
          Script.Sub -> pure $ VInt (a' - b')
          Script.Div ->
            if b' == 0
              then throwError DivideByZero
              else pure $ VInt (a' `div` b')
          Script.Mul     -> pure $ VInt (a' * b')
          Script.Equal   -> pure $ VBool $ a' == b'
          Script.NEqual  -> pure $ VBool $ a' /= b'
          Script.LEqual  -> pure $ VBool $ a' <= b'
          Script.GEqual  -> pure $ VBool $ a' >= b'
          Script.Lesser  -> pure $ VBool $ a' < b'
          Script.Greater -> pure $ VBool $ a' > b'
          _ -> binOpFail
      (VFloat a', VFloat b') -> evalBinOpF op VFloat a' b'
      (VFixed a', VFixed b') ->
        case (a',b') of
          (Fixed1 x, Fixed1 y) -> evalBinOpF op (VFixed . Fixed1) x y
          (Fixed2 x, Fixed2 y) -> evalBinOpF op (VFixed . Fixed2) x y
          (Fixed3 x, Fixed3 y) -> evalBinOpF op (VFixed . Fixed3) x y
          (Fixed4 x, Fixed4 y) -> evalBinOpF op (VFixed . Fixed4) x y
          (Fixed5 x, Fixed5 y) -> evalBinOpF op (VFixed . Fixed5) x y
          (Fixed6 x, Fixed6 y) -> evalBinOpF op (VFixed . Fixed6) x y
          (_,_) -> binOpFail
      (VDateTime (DateTime dt), VTimeDelta (TimeDelta d)) ->
        case op of
          Script.Add -> pure $ VDateTime $ DateTime $ add dt d
          Script.Sub -> pure $ VDateTime $ DateTime $ sub dt d
          _ -> binOpFail
      (VTimeDelta (TimeDelta d), VDateTime (DateTime dt)) ->
        case op of
          Script.Add -> pure $ VDateTime $ DateTime $ add dt d
          Script.Sub -> pure $ VDateTime $ DateTime $ sub dt d
          _ -> binOpFail
      (VTimeDelta (TimeDelta d1), VTimeDelta (TimeDelta d2)) ->
        case op of
          Script.Add -> pure $ VTimeDelta $ TimeDelta $ d1 <> d2
          Script.Sub -> pure $ VTimeDelta $ TimeDelta $ subDeltas d1 d2
          _ -> binOpFail
      (VTimeDelta (TimeDelta d), VInt n) ->
        case op of
          Script.Mul ->
            case scaleDelta (fromIntegral n) d of
              Nothing -> binOpFail -- XXX More descriptive error
              Just newDelta -> pure $ VTimeDelta $ TimeDelta newDelta
          _ -> binOpFail
      (VBool a', VBool b') -> return $
        case op of
          Script.And -> VBool (a' && b')
          Script.Or  -> VBool (a' || b')
          Script.Equal -> VBool $ a' == b'
          Script.NEqual -> VBool $ a' /= b'
          _ -> binOpFail
      (VAccount a', VAccount b') -> return $
        case op of
          Script.Equal -> VBool $ a' == b'
          Script.NEqual -> VBool $ a' /= b'
          _ -> binOpFail
      (VAsset a', VAsset b') -> return $
        case op of
          Script.Equal -> VBool $ a' == b'
          Script.NEqual -> VBool $ a' /= b'
          _ -> binOpFail
      (VContract a', VContract b') -> return $
        case op of
          Script.Equal -> VBool $ a' == b'
          Script.NEqual -> VBool $ a' /= b'
          _ -> binOpFail
      (VDateTime a', VDateTime b') -> return $
        case op of
          Script.Equal -> VBool $ a' == b'
          Script.NEqual -> VBool $ a' /= b'
          Script.LEqual -> VBool $ a' <= b'
          Script.GEqual -> VBool $ a' >= b'
          Script.Lesser -> VBool $ a' < b'
          Script.Greater -> VBool $ a' > b'
          _ -> binOpFail
      (VMsg a', VMsg b') -> return $
        case op of
          Script.Equal -> VBool $ a' == b'
          Script.NEqual -> VBool $ a' /= b'
          Script.LEqual -> VBool $ a' <= b'
          Script.GEqual -> VBool $ a' >= b'
          Script.Lesser -> VBool $ a' < b'
          Script.Greater -> VBool $ a' > b'
          Script.Add -> VMsg $ a' <> b'
          _ -> binOpFail
      (v1, v2) -> panicImpossible $ Just $
        "evalLExpr EBinOp: (" <> show v1 <> ", " <> show v2 <> ")"

  EVar (Located _ var) -> do
    mVal <- lookupVar var
    case mVal of
      Nothing -> do
        isLocal <- isLocalVar var
        if isLocal
          then throwError $ LocalVarNotFound var
          else panicImpossible $ Just "evalLExpr: EVar"
      Just val -> return val

  ECall ef args   ->
    case ef of
      Left primOp -> evalPrim primOp args
      Right hnm   -> do
        helper <- lookupHelper hnm
        argVals <- mapM evalLExpr args
        let argNmsAndVals =
               map (first (locVal . argName)) $
                 zip (helperArgs helper) argVals
        localTempStorage argNmsAndVals $ do
          evalLExpr (helperBody helper)

  EBefore dt e -> do
    now <- currentTimestamp <$> ask
    let noLoc = Located NoLoc
    let nowDtLLit  = noLoc $ LDateTime $ DateTime $ posixMicroSecsToDatetime now
    let predicate = EBinOp (noLoc LEqual) (noLoc $ ELit nowDtLLit) dt
    evalLExpr $ noLoc $ EIf (noLoc predicate) e (noLoc ENoOp)

  EAfter dt e -> do
    now <- currentTimestamp <$> ask
    let noLoc = Located NoLoc
    let nowDtLLit  = noLoc $ LDateTime $ DateTime $ posixMicroSecsToDatetime now
    let predicate = EBinOp (noLoc GEqual) (noLoc $ ELit nowDtLLit) dt
    evalLExpr $ noLoc $ EIf (noLoc predicate) e (noLoc ENoOp)

  EBetween startDte endDte e -> do
    now <- currentTimestamp <$> ask
    let noLoc = Located NoLoc
    let nowDtLExpr = noLoc $ ELit $ noLoc $
          LDateTime $ DateTime $ posixMicroSecsToDatetime now
    VBool b <- evalPrim Between [nowDtLExpr, startDte, endDte]
    if b
      then evalLExpr e
      else noop

  EIf cond e1 e2 -> do
    VBool b <- evalLExpr cond
    if b
      then evalLExpr e1
      else evalLExpr e2

  ECase scrut ms -> do
    VEnum c <- evalLExpr scrut
    evalLExpr (match ms c)

  ENoOp       -> noop

match :: [Match] -> EnumConstr -> LExpr
match ps c
  = fromMaybe (panicImpossible $ Just "Cannot match constructor")
  $ List.lookup (PatLit c)
  $ map (\(Match pat body) -> (locVal pat, body))
  $ ps

-- | Evaluate a binop and two Fractional Num args
evalBinOpF :: (Fractional a, Ord a) => BinOp -> (a -> Value) -> a -> a -> EvalM Value
evalBinOpF Script.Add constr a b = pure $ constr (a + b)
evalBinOpF Script.Sub constr a b = pure $ constr (a - b)
evalBinOpF Script.Mul constr a b = pure $ constr (a * b)
evalBinOpF Script.Div constr a b
  | b == 0 = throwError DivideByZero
  | otherwise = pure $ constr (a / b)
evalBinOpF Script.Equal constr a b = pure $ VBool (a == b)
evalBinOpF Script.NEqual constr a b = pure $ VBool (a /= b)
evalBinOpF Script.LEqual constr a b = pure $ VBool (a <= b)
evalBinOpF Script.GEqual constr a b = pure $ VBool (a >= b)
evalBinOpF Script.Lesser constr a b = pure $ VBool (a < b)
evalBinOpF Script.Greater constr a b = pure $ VBool (a > b)
evalBinOpF bop c a b = panicInvalidBinOp bop (c a) (c b)

evalPrim :: PrimOp -> [LExpr] -> EvalM Value
evalPrim ex args = case ex of
  Now               -> do
    currDatetime <- posixMicroSecsToDatetime . currentTimestamp <$> ask
    pure $ VDateTime $ DateTime currDatetime
  Block             -> VInt . currentBlock <$> ask
  Deployer          -> VAccount . currentDeployer <$> ask
  Sender            -> VAccount . currentTxIssuer <$> ask
  Created           -> do
    createdDatetime <- posixMicroSecsToDatetime . currentCreated <$> ask
    pure $ VDateTime $ DateTime createdDatetime
  Address           -> VContract . currentAddress <$> ask
  Validator         -> VAccount . currentValidator <$> ask

  Fixed1ToFloat  -> do
    let [eFixed] = args
    VFixed (Fixed1 (F1 n)) <- evalLExpr eFixed
    pure $ VFloat $ read $ showFixed False n

  Fixed2ToFloat  -> do
    let [eFixed] = args
    VFixed (Fixed2 (F2 n)) <- evalLExpr eFixed
    pure $ VFloat $ read $ showFixed False n

  Fixed3ToFloat  -> do
    let [eFixed] = args
    VFixed (Fixed3 (F3 n)) <- evalLExpr eFixed
    pure $ VFloat $ read $ showFixed False n

  Fixed4ToFloat  -> do
    let [eFixed] = args
    VFixed (Fixed4 (F4 n)) <- evalLExpr eFixed
    pure $ VFloat $ read $ showFixed False n

  Fixed5ToFloat  -> do
    let [eFixed] = args
    VFixed (Fixed5 (F5 n)) <- evalLExpr eFixed
    pure $ VFloat $ read $ showFixed False n

  Fixed6ToFloat  -> do
    let [eFixed] = args
    VFixed (Fixed6 (F6 n)) <- evalLExpr eFixed
    pure $ VFloat $ read $ showFixed False n

  FloatToFixed1  -> evalFloatToFixed Prec1 args
  FloatToFixed2  -> evalFloatToFixed Prec2 args
  FloatToFixed3  -> evalFloatToFixed Prec3 args
  FloatToFixed4  -> evalFloatToFixed Prec4 args
  FloatToFixed5  -> evalFloatToFixed Prec5 args
  FloatToFixed6  -> evalFloatToFixed Prec6 args

  Terminate -> do
    let [Located l (LMsg msg)] = argLits (fmap unLoc args)
    emitDelta $ Delta.ModifyState GraphTerminal
    emitDelta $ Delta.Terminate $ SS.toBytes msg
    updateState GraphTerminal
    pure VVoid

  Transition     -> do
    let [Located l (LState label)] = argLits (fmap unLoc args)
    emitDelta $ Delta.ModifyState (GraphLabel label)
    updateState (GraphLabel label)
    pure VVoid

  CurrentState   -> do
    gst <- getState
    case gst of
      GraphTerminal    -> pure (VState terminalLabel)
      GraphInitial     -> pure (VState initialLabel)
      GraphLabel label -> pure (VState label)

  Sign           -> do
    let [msgExpr] = args
    (VMsg msg) <- evalLExpr msgExpr
    privKey <- currentPrivKey <$> ask -- XXX               V gen Random value?
    sig <- Key.getSignatureRS <$> Key.sign privKey (SS.toBytes msg)
    case bimap toSafeInteger toSafeInteger sig of
      (Right safeR, Right safeS) -> return $ VSig (safeR,safeS)
      otherwise -> throwError $
        HugeInteger "Signature values (r,s) too large."

  Sha256         -> do
    let [anyExpr] = args
    x <- evalLExpr anyExpr
    v <- hashValue x
    case SS.fromBytes (Hash.sha256Raw v) of
      Left err -> throwError $ HugeString $ show err
      Right msg -> return $ VMsg msg

  AccountExists  -> do
    let [varExpr] = args
    accAddr <- extractAddrAccount <$> evalLExpr varExpr
    world <- gets worldState
    return $ VBool $ Ledger.accountExists accAddr world

  AssetExists    -> do
    let [varExpr] = args
    assetAddr <- extractAddrAsset <$> evalLExpr varExpr
    world <- gets worldState
    return $ VBool $ Ledger.assetExists assetAddr world

  ContractExists -> do
    let [varExpr] = args
    contractAddr <- extractAddrContract <$> evalLExpr varExpr
    world <- gets worldState
    return $ VBool $ Ledger.contractExists contractAddr world

  Verify         -> do
    let [accExpr,sigExpr,msgExpr] = args
    (VSig safeSig) <- evalLExpr sigExpr
    (VMsg msg) <- evalLExpr sigExpr
    ledgerState <- gets worldState

    acc <- getAccount accExpr
    let sig = bimap fromSafeInteger fromSafeInteger safeSig
    return $ VBool $
      Key.verify (publicKey acc) (Key.mkSignatureRS sig) $ SS.toBytes msg

  TxHash -> do
    hash <- currentTransaction <$> ask
    case SS.fromBytes (Hash.getRawHash hash) of
      Left err -> throwError $ HugeString $ show err
      Right msg -> pure $ VMsg msg

  ContractValue -> do
    let [contractExpr, msgExpr] = args
    contractAddr <- extractAddrContract <$> evalLExpr contractExpr
    world <- gets worldState
    case Ledger.lookupContract contractAddr world of
      Left err -> throwError $ ContractIntegrity $ show err
      Right contract -> do
        (VMsg varSS) <- evalLExpr msgExpr
        let var = toS $ SS.toBytes varSS
        case Contract.lookupVarGlobalStorage var contract of
          Nothing -> throwError $ ContractIntegrity $
            "Contract does not define a variable named '" <> var <> "'"
          Just val -> pure val

  ContractValueExists -> do
    -- If ContractValue throws err, value doesn't exist
    flip catchError (const $ pure $ VBool False) $ do
      _ <- evalPrim ContractValue args
      pure $ VBool True

  ContractState -> do
    let [contractExpr] = args
    contractAddr <- extractAddrContract <$> evalLExpr contractExpr
    world <- gets worldState
    case Ledger.lookupContract contractAddr world of
      Left err -> throwError $ ContractIntegrity $ show err
      Right contract -> pure $ VState $
        case Contract.state contract of
          GraphInitial     -> Script.Graph.initialLabel
          GraphTerminal    -> Script.Graph.terminalLabel
          GraphLabel label -> label

  NovationInit -> do
    let [timeout] = args
    VInt timeout' <- evalLExpr timeout
    sideInit timeout'
    noop

  NovationStop -> do
    sideStop
    noop

  -- Datetime manipulation prim ops

  IsBusinessDayUK -> do
    let [dateTimeExpr] = args
    VDateTime (DateTime dt) <- evalLExpr dateTimeExpr
    return $ VBool $ DT.isBusiness DT.ukHolidays dt

  NextBusinessDayUK -> do
    let [dateTimeExpr] = args
    VDateTime (DateTime dt) <- evalLExpr dateTimeExpr
    return $ VDateTime $ DateTime $ DT.nextBusinessDay DT.ukHolidays dt

  IsBusinessDayNYSE -> do
    let [dateTimeExpr] = args
    VDateTime (DateTime dt) <- evalLExpr dateTimeExpr
    return $ VBool $ DT.isBusiness DT.nyseHolidays dt

  NextBusinessDayNYSE -> do
    let [dateTimeExpr] = args
    VDateTime (DateTime dt) <- evalLExpr dateTimeExpr
    return $ VDateTime $ DateTime $ DT.nextBusinessDay DT.nyseHolidays dt

  Between -> do
    let [dtExpr, startExpr, endExpr] = args
    VDateTime (DateTime dt) <- evalLExpr dtExpr
    VDateTime (DateTime start) <- evalLExpr startExpr
    VDateTime (DateTime end) <- evalLExpr endExpr
    return $ VBool $ within dt (Interval start end)

  TimeDiff -> do
    [VDateTime (DateTime dt), VDateTime (DateTime dt')] <- mapM evalLExpr args
    pure $ VTimeDelta (TimeDelta (DT.diff dt dt'))

  AssetPrimOp a -> evalAssetPrim a args
  MapPrimOp m   -> evalMapPrim m args
  SetPrimOp m   -> evalSetPrim m args
  CollPrimOp c  -> evalCollPrim c args

  Bound -> notImplemented -- XXX

evalAssetPrim :: Prim.AssetPrimOp -> [LExpr] -> EvalM Value
evalAssetPrim assetPrimOp args =
  case assetPrimOp of

    -- May return VInt, VBool, or VFixed depending on asset type
    Prim.HolderBalance -> do
      let [assetExpr, accExpr] = args
      asset <- getAsset assetExpr
      accAddr <- getAccountAddr accExpr
      case Asset.assetType asset of
        Asset.Discrete  ->
          case Asset.balance asset (Asset.Holder accAddr) of
            Nothing  -> return $ VInt 0
            Just bal -> return $ VInt bal
        Asset.Fractional n ->
          case Asset.balance asset (Asset.Holder accAddr) of
            Nothing  -> return $ VFloat 0.0
            Just bal -> return $ VFloat $
              -- normalize the holdings amount by `bal * 10^(-n)`
              fromIntegral bal * 10**(fromIntegral $ negate $ fromEnum n + 1)
        Asset.Binary ->
          case Asset.balance asset (Asset.Holder accAddr) of
            Nothing  -> return $ VBool False
            Just bal -> return $ VBool True

    -- From Account to Contract
    Prim.TransferTo  -> do
      let [assetExpr,holdingsExpr] = args

      senderAddr <- currentTxIssuer <$> ask
      contractAddr <- currentAddress <$> ask
      assetAddr <- getAssetAddr assetExpr

      -- Eval and convert holdings val to integer
      holdingsVal <- evalLExpr holdingsExpr
      let holdings = holdingsValToInteger holdingsVal

      -- Modify the world (perform the transfer)
      world <- gets worldState
      case Ledger.transferAsset assetAddr (Asset.Holder senderAddr) (Asset.Holder contractAddr) holdings world of
        Left err -> throwError $ AssetIntegrity $ show err
        Right newWorld -> setWorld newWorld

      -- Emit the delta denoting the world state modification
      emitDelta $ Delta.ModifyAsset $
        Delta.TransferTo assetAddr holdings senderAddr contractAddr

      noop

    -- From Contract to Account
    Prim.TransferFrom  -> do
      let [assetExpr,holdingsExpr,accExpr] = args

      contractAddr <- currentAddress <$> ask
      assetAddr <- getAssetAddr assetExpr
      accAddr <- getAccountAddr accExpr

      -- Eval and convert holdings val to integer
      holdingsVal <- evalLExpr holdingsExpr
      let holdings = holdingsValToInteger holdingsVal

      -- Modify the world (perform the transfer)
      world <- gets worldState
      case Ledger.transferAsset assetAddr (Asset.Holder contractAddr) (Asset.Holder accAddr) holdings world of
        Left err -> throwError $ AssetIntegrity $ show err
        Right newWorld -> setWorld newWorld

      -- Emit the delta denoting the world state modification
      emitDelta $ Delta.ModifyAsset $
        Delta.TransferFrom assetAddr holdings accAddr contractAddr

      noop

    -- Circulate the supply of an Asset to the Asset issuer's holdings
    Prim.CirculateSupply -> do
      let [assetExpr, amountExpr] = args

      assetAddr <- getAssetAddr assetExpr

      -- Eval and convert amount val to integer
      amountVal <- evalLExpr amountExpr
      let amount = holdingsValToInteger amountVal

      -- Perform the circulation
      world <- gets worldState
      txIssuer <- asks currentTxIssuer
      case Ledger.circulateAsset assetAddr txIssuer amount world of
        Left err -> throwError $ AssetIntegrity $ show err
        Right newWorld -> setWorld newWorld

      noop

    -- From Account to Account
    Prim.TransferHoldings -> do
      let [fromExpr,assetExpr,holdingsExpr,toExpr] = args

      assetAddr <- getAssetAddr assetExpr
      fromAddr <- getAccountAddr fromExpr
      toAddr <- getAccountAddr toExpr

      -- Eval and convert holdings val to integer
      holdingsVal <- evalLExpr holdingsExpr
      let holdings = holdingsValToInteger holdingsVal

      -- Modify the world (perform the transfer)
      world <- gets worldState
      case Ledger.transferAsset assetAddr (Asset.Holder fromAddr) (Asset.Holder toAddr) holdings world of
        Left err -> throwError $ AssetIntegrity $ show err
        Right newWorld -> setWorld newWorld

      -- Emit the delta denoting the world state modification
      emitDelta $ Delta.ModifyAsset $
        Delta.TransferHoldings fromAddr assetAddr holdings toAddr

      noop

  where
    holdingsValToInteger :: Value -> Int64
    holdingsValToInteger = \case
      VInt n   -> n
      VBool b  -> if b then 1 else 0
      VFixed f -> fromIntegral $ getFixedInteger f
      otherVal -> panicInvalidHoldingsVal otherVal

evalMapPrim :: Prim.MapPrimOp -> [LExpr] -> EvalM Value
evalMapPrim mapPrimOp args =
  case mapPrimOp of
    Prim.MapInsert -> do
      [k, v, VMap mapVal] <- mapM evalLExpr args
      pure $ VMap (Map.insert k v mapVal)
    Prim.MapDelete -> do
      [k, VMap mapVal] <- mapM evalLExpr args
      pure $ VMap (Map.delete k mapVal)
    Prim.MapLookup -> do
      [k, VMap mapVal] <- mapM evalLExpr args
      case Map.lookup k mapVal of
        Nothing -> throwError $ LookupFail (show k)
        Just v  -> pure v
    Prim.MapModify -> do
      let [kexpr, Located _ (EVar lnm), mexpr] = args
      k <- evalLExpr kexpr
      VMap mapVal <- evalLExpr mexpr
      case Map.lookup k mapVal of
        Nothing -> throwError $ ModifyFail (show k)
        Just v  -> do
          helper <- lookupHelper lnm
          -- arity check passed in typechecker
          let [harg] = map (locVal . argName) (helperArgs helper)
          newVal <- localTempStorage [(harg,v)] $ evalLExpr (helperBody helper)
          pure $ VMap (Map.insert k newVal mapVal)

evalSetPrim :: Prim.SetPrimOp -> [LExpr] -> EvalM Value
evalSetPrim setPrimOp args =
  case setPrimOp of
    Prim.SetInsert -> do
      [v, VSet setVal] <- mapM evalLExpr args
      pure $ VSet (Set.insert v setVal)
    Prim.SetDelete -> do
      [v, VSet setVal] <- mapM evalLExpr args
      pure $ VSet (Set.delete v setVal)

evalCollPrim :: Prim.CollPrimOp -> [LExpr] -> EvalM Value
evalCollPrim collPrimOp args =
  case collPrimOp of
    Prim.Aggregate -> do
      let [vExpr, Located _ (EVar lnm), collExpr] = args
      initVal <- evalLExpr vExpr
      helper <- lookupHelper lnm
      collVal <- evalLExpr collExpr
      let [hargNm1, hargNm2] = map (locVal . argName) (helperArgs helper)
          foldColl' = foldColl (helperBody helper) (hargNm1, initVal) hargNm2
      case collVal of
        -- Currently, fold is performed over map vals ordered by keys
        VMap vmap -> foldColl' (Map.elems vmap)
        VSet vset -> foldColl' (Set.toList vset)
        otherwise -> throwInvalidCollErr (located collExpr) collVal
    Prim.Transform -> do
      let [Located _ (EVar lnm), collExpr] = args
      helper <- lookupHelper lnm
      collVal <- evalLExpr collExpr
      let [hargNm] = map (locVal . argName) (helperArgs helper)
      case collVal of
        VMap vmap -> VMap <$> mapColl (helperBody helper) hargNm vmap
        VSet vset -> VSet . Set.fromList <$>
          mapColl (helperBody helper) hargNm (Set.toList vset)
        otherwise -> throwInvalidCollErr (located collExpr) collVal
    Prim.Filter -> do
      let [Located _ (EVar lnm), collExpr] = args
      helper <- lookupHelper lnm
      collVal <- evalLExpr collExpr
      let [hargNm] = map (locVal . argName) (helperArgs helper)
      case collVal of
        VMap vmap ->
          fmap (VMap . Map.fromList) $
            flip filterM (Map.toList vmap) $ \(k,v) ->
              filterPred (helperBody helper) (hargNm,v)
        VSet vset ->
          fmap (VSet . Set.fromList) $
            flip filterM (Set.toList vset) $ \v ->
              filterPred (helperBody helper) (hargNm,v)
        otherwise -> throwInvalidCollErr (located collExpr) collVal
    Prim.Element -> do
      let [vExpr, collExpr] = args
      val <- evalLExpr vExpr
      collVal <- evalLExpr collExpr
      case collVal of
        VMap vmap -> pure (VBool (val `elem` vmap))
        VSet vset -> pure (VBool (val `elem` vset))
        otherwise -> throwInvalidCollErr (located collExpr) collVal
    Prim.IsEmpty -> do
      let [collExpr] = args
      collVal <- evalLExpr collExpr
      case collVal of
        VMap vmap -> pure (VBool (Map.null vmap))
        VSet vset -> pure (VBool (Set.null vset))
        otherwise -> throwInvalidCollErr (located collExpr) collVal
  where
    throwInvalidCollErr loc v = throwError $
      CallPrimOpFail loc v "Cannot call a collection primop on a non-collection value"

    -- Map over a collection type (which happen to all implement Traversable)
    mapColl :: Traversable f => LExpr -> Name -> f Value -> EvalM (f Value)
    mapColl body nm coll =
      forM coll $ \val ->
        localTempStorage [(nm, val)] (evalLExpr body)

    filterPred :: LExpr -> (Name, Value) -> EvalM Bool
    filterPred body var = do
      res <- localTempStorage [var] (evalLExpr body)
      pure $ case res of
        VBool True  -> True
        VBool False -> False
        otherwise -> panicImpossible $ Just "Body of helper function used in filter primop did not return Bool"

    foldColl :: LExpr -> (Name, Value) -> Name -> [Value] -> EvalM Value
    foldColl fbody (accNm, initVal) argNm vals =
        foldM accum initVal vals
      where
        accum accVal v =
          localTempStorage
            [(accNm, accVal), (argNm, v)]
            (evalLExpr fbody)

getAccountAddr :: LExpr -> EvalM (Address AAccount)
getAccountAddr accExpr =
  Account.address <$> getAccount accExpr

getAccount :: LExpr -> EvalM Account.Account
getAccount accExpr = do
  ledgerState <- gets worldState
  accAddr <- extractAddrAccount <$> evalLExpr accExpr
  case Ledger.lookupAccount accAddr ledgerState of
    Left err  -> throwError $
      AccountIntegrity ("No account with address: " <> show accAddr)
    Right acc -> pure acc

getAssetAddr :: LExpr -> EvalM (Address AAsset)
getAssetAddr assetExpr =
  Asset.address <$> getAsset assetExpr

getAsset :: LExpr -> EvalM Asset.Asset
getAsset assetExpr = do
  ledgerState <- gets worldState
  assetAddr   <- extractAddrAsset <$> evalLExpr assetExpr
  case Ledger.lookupAsset assetAddr ledgerState of
    Left err    -> throwError $
      AssetIntegrity ("No asset with address: " <> show assetAddr)
    Right asset -> pure asset

checkSideGraph :: Method -> EvalM ()
checkSideGraph meth = do
  (locked, ~(Just (lockStart, delta))) <- gets sideLock
  case locked of
    True  -> do
      now <- currentTimestamp <$> ask
      -- lock timeout
      if now > (lockStart + delta)
        then sideUnlock
        else do
          -- subgraph check
          case methodTag meth of
            Subg _ -> pure () -- we're in subgraph
            Main _ -> throwError SubgraphLock
    False -> pure ()

-- | Check graph state is legal.
checkGraph :: Method -> EvalM ()
checkGraph meth = do
  st <- getState
  case methodTag meth of
    Subg _ -> panic "Impossible" -- XXX: fix me
    Main tag -> do
      when (st /= handleTag tag) $
        throwError $ InvalidState tag st
  where
    handleTag :: Label -> GraphState
    handleTag "initial" = GraphInitial
    handleTag "terminal" = GraphTerminal
    handleTag lab = GraphLabel lab

checkAccess :: Method -> EvalM ()
checkAccess m = do
    case methodAccess m of
      RoleAny -> return ()
      RoleAnyOf rs -> do
        group <- mapM (valToAccount <=< evalLExpr) rs
        issuer <- currentTxIssuer <$> ask
        unless
          (issuer `elem` group)
          (throwError $ NotAuthorisedError m issuer)
  where
    valToAccount :: Value -> EvalM (Address AAccount)
    valToAccount (VAccount aaddr) = return aaddr
    valToAccount x = throwError $ Impossible "Type checker ensures that this is an account."

-- | Does not perform typechecking on args supplied, eval should only happen
-- after typecheck.
evalMethod :: Method -> [Value] -> EvalM Value
evalMethod meth @ (Method _ access nm argTyps body) args
  | numArgs /= numArgsGiven = throwError $
      MethodArityError nm numArgs numArgsGiven
  | otherwise = do
      checkAccess meth
      -- Sidegraph precondition check
      checkSideGraph meth
      checkGraph meth
      let argNms = map (\(Arg _ lname) -> locVal lname) argTyps
      forM_ (zip argNms args) $ uncurry $ insertTempVar
      evalLExpr body
  where
    numArgs = length argTyps
    numArgsGiven = length args

-- | Evaluation entry
-- Methods will only ever be evaluated in the context of a contract on the
-- ledger. If script methods should be evaluated outside of the context of a
-- contract, call `evalMethod`.
eval :: Contract -> Name -> [Value] -> EvalM Value
eval c nm args =
  case Contract.lookupContractMethod nm c of
    Right method -> evalMethod method args
    Left err -> throwError (InvalidMethodName err)

evalFloatToFixed :: PrecN -> [LExpr] -> EvalM Value
evalFloatToFixed prec args = do
    let [eFloat] = args
    VFloat float <- evalLExpr eFloat
    pure $ VFixed $ floatToFixed prec float
  where
    floatToFixed :: PrecN -> Double -> FixedN
    floatToFixed Prec1 = Fixed1 . F1 . MkFixed . round . (*) (10^1)
    floatToFixed Prec2 = Fixed2 . F2 . MkFixed . round . (*) (10^2)
    floatToFixed Prec3 = Fixed3 . F3 . MkFixed . round . (*) (10^3)
    floatToFixed Prec4 = Fixed4 . F4 . MkFixed . round . (*) (10^4)
    floatToFixed Prec5 = Fixed5 . F5 . MkFixed . round . (*) (10^5)
    floatToFixed Prec6 = Fixed6 . F6 . MkFixed . round . (*) (10^6)

noop :: EvalM Value
noop = pure VVoid


-- XXX find a better way to do this
extractAddrAccount :: Value -> Address AAccount
extractAddrAccount (VAccount addr) = addr
extractAddrAccount _ = panicImpossible $ Just "extractAddrAccount"

extractAddrAsset :: Value -> Address AAsset
extractAddrAsset (VAsset addr) = addr
extractAddrAsset _ = panicImpossible $ Just "extractAddrAsset"

extractAddrContract :: Value -> Address AContract
extractAddrContract (VContract addr) = addr
extractAddrContract _ = panicImpossible $ Just "extractAddrContract"

-------------------------------------------------------------------------------
-- Value Hashing
-------------------------------------------------------------------------------

{-# INLINE hashValue #-}
hashValue :: Value -> EvalM ByteString
hashValue = \case
  VMsg msg       -> pure $ SS.toBytes msg
  VInt n         -> pure (show n)
  VFloat n       -> pure (show n)
  VFixed n       -> pure (show n)
  VBool n        -> pure (show n)
  VState n       -> pure (show n)
  VAccount a     -> pure (rawAddr a)
  VContract a    -> pure (rawAddr a)
  VAsset a       -> pure (rawAddr a)
  VVoid          -> pure ""
  VDateTime dt   -> pure $ S.encode dt
  VTimeDelta d   -> pure $ S.encode d
  VEnum c        -> pure (show c)
  VMap vmap      -> pure (show vmap)
  VSet vset      -> pure (show vset)
  VSig _         -> throwError $ Impossible "Cannot hash signature"
  VUndefined     -> throwError $ Impossible "Cannot hash undefined"

-------------------------------------------------------------------------------
  -- Eval specific fatal errors
-------------------------------------------------------------------------------

panicInvalidBinOp :: BinOp -> Value -> Value -> a
panicInvalidBinOp op x y = panicImpossible $ Just $
  "Operator " <> show op <> " cannot be used with " <> show x <> " and " <> show y

panicInvalidUnOp :: UnOp -> Value -> a
panicInvalidUnOp op x = panicImpossible $ Just $
  "Operator " <> show op <> " cannot be used with " <> show x

panicInvalidHoldingsVal :: Value -> a
panicInvalidHoldingsVal v = panicImpossible $ Just $
  "Only VInt, VBool, and VFixed can be values of asset holdings. Instead, saw: "  <> show v
