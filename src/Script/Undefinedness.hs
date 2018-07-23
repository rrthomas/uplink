{-|

Analyse scripts for references to undefined variables.

The idea is to abstractly interpret sequences of FCL script method
calls where variables can either be initialized, uninitialized or in
an error state.

We generate "stack traces" (sequences of method calls) from the script
graph that cover all edges. (Note that there might be more edges than
there are methods, as a method may have transitions to several
distinct states.)

The collection of stack traces we consider should be complete in the
sense that every path is there, modulo walking loops. (This statement
is however not formally proven, so it might not actually hold.)

For every stack trace, we check whether it may give rise to references
to uninitialized variables. If this is not the case for any of the
stack traces, the undefinedness check passes.

Things the analysis currently does not deal with:

 * novation
 * local variables (are assumed to be initialised)

-}

{-# LANGUAGE TupleSections #-}

module Script.Undefinedness (
  -- * Undefinedness analysis
  undefinednessAnalysis
  -- * Stack traces
  , StackTrace
  , StackTraceItem(..)
  -- * Domain of analysis
  , IsInitialized(..)
  , meet
  -- * Environment/state used in analysis
  , UndefinednessEnv
  , envMeet
  , envMeets
  , initializeInEnv
  , initialEnv
  -- * Types of partial and end result of analysis
  , UndefinednessResult
  , InvalidStackTrace(..)
  , ValidStackTrace(..)
  -- * Stack trace, expression and statement validation
  , checkExpression
  , checkStatement
  , fetchScriptMethods
  , validateStackTrace
  ) where

import Protolude

import Control.Arrow ((&&&))
import Control.Exception.Base (assert)
import qualified Data.Either as Either
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import Script
import qualified Script.Analysis as GraphAnalysis
import Script.Pretty hiding ((<>))
import qualified Script.Prim as Prim

-- | Run the undefinedness analysis.
undefinednessAnalysis
  :: Script
  -> Either [InvalidStackTrace] [ValidStackTrace]
undefinednessAnalysis scr = do
  initEnv <- initialEnv scr
  case allErrors initEnv of
    [] -> Right $ allSuccesses initEnv
    errs@(_:_) -> Left errs
  where
    methods = fetchScriptMethods scr
    allStackTraces = generateStackTraces scr
    allResults env = map (validateStackTrace env methods) allStackTraces
    allErrors env = Either.lefts (allResults env)
    allSuccesses env = Either.rights (allResults env)

-------------------------------------------------------------------------------
-- Stack traces
-------------------------------------------------------------------------------

-- | A "stack trace" is essentially a path through the script
-- graph. Any such path is assumed to start from the "initial" state.
type StackTrace = [StackTraceItem]

-- | Since a stack trace is assumed to start from the "initial" state,
-- we only store a list of destinations and method names (edge
-- labels).
data StackTraceItem
  = StackTraceItem
  { stackTraceMeth :: Name
  , stackTraceSrc :: GraphLabel
  , stackTraceDest :: GraphLabel
  }
  deriving (Show, Eq)

-------------------------------------------------------------------------------
-- The "domain" of the abstract interpretation
-------------------------------------------------------------------------------

-- | A variable may either be initialized, uninitialized or in an
-- error state (e.g. it has been assigned the value of an
-- uninitialized variable).
data IsInitialized
  = Initialized
  | Uninitialized
  | Error (Set Loc)
    deriving Show

-- | Combining initialisation information means that we approximate
-- "downwards": if one of the two variables is in an error state, the
-- combination will be in an error state.
meet :: IsInitialized -> IsInitialized -> IsInitialized
meet (Error x) (Error y) = Error (x `Set.union` y)
meet e@(Error _) _ = e
meet _ e@(Error _) = e
meet Uninitialized Uninitialized = Uninitialized
meet Uninitialized Initialized = Uninitialized
meet Initialized Uninitialized = Uninitialized
meet Initialized Initialized = Initialized

-------------------------------------------------------------------------------
-- Environment/state used in analysis
-------------------------------------------------------------------------------

-- | The environment we work with maps variable names to the
-- initialized status.
type UndefinednessEnv = Map Name IsInitialized

-- | Combine environments, performing a meet when on values of
-- overlapping keys.
envMeet :: UndefinednessEnv -> UndefinednessEnv -> UndefinednessEnv
envMeet = Map.unionWith meet

-- | Combine many environments
envMeets :: [UndefinednessEnv] -> UndefinednessEnv
envMeets = Map.unionsWith meet

-- | Add a variable and its undefinedness status to the environment,
-- unless there is already an error there.
--
-- (Motivation: if we have an assignment for variable x whose rhs
-- refers to an uninitialized variable and a later assignment that
-- assigns a literal to the same variable x, the variable x should
-- still be in an error state. Later valid statements should not make
-- us forget earlier invalid statements.)
initializeInEnv
  :: Name
  -> IsInitialized
  -> UndefinednessEnv
  -> UndefinednessEnv
initializeInEnv = Map.insertWith replaceUnlessError
  where
    -- New value should be inserted unless the old value is an
    -- error. (This is not a "meet" as this operation is allowed to
    -- change an uninitialized value into an initialized one.)
    replaceUnlessError new@(Error _) _old = new
    replaceUnlessError _new old@(Error _) = old
    replaceUnlessError new _old = new

-- | Given the initial values of the global and local variables (or
-- lack thereof), set their undefinedness status in the environment
-- accordingly.
initialEnv :: Script -> Either [InvalidStackTrace] UndefinednessEnv
initialEnv = handleErrors . foldlM addDef Map.empty . scriptDefs
  where
    handleErrors
      :: Either Text UndefinednessEnv
      -> Either [InvalidStackTrace] UndefinednessEnv
    handleErrors (Left err)
      = Left [InvalidStackTrace [] [err]]
    handleErrors (Right env)
      = case collectErrors env of
          [] -> pure env
          errs@(_:_) -> Left [InvalidStackTrace [] errs]

    addDef oldEnv (GlobalDef _ _ n lexpr)
      = checkAssignment (located lexpr) oldEnv n lexpr
    addDef oldEnv (GlobalDefNull _ _ ln)
      = pure $ Map.insert (locVal ln) Uninitialized oldEnv
    addDef oldEnv (LocalDef _ n lexpr)
      = checkAssignment (located lexpr) oldEnv n lexpr
    -- XXX always initialize local definitions because Gabe does not
    -- understand them :(
    addDef oldEnv (LocalDefNull _ ln)
      = pure $ Map.insert (locVal ln) Initialized oldEnv

-------------------------------------------------------------------------------
-- Types of partial and end results of analysis
-------------------------------------------------------------------------------

-- | The result of the analysis is either a valid stack trace in which
-- there are no references to uninitialized variables, or an invalid
-- stack trace that tells us what went wrong at which point in the
-- contract.
type UndefinednessResult
  = Either InvalidStackTrace ValidStackTrace

-- | Invalid stack trace
data InvalidStackTrace
  = InvalidStackTrace
    { invalidStackTrace :: StackTrace
    -- ^ Path from "initial" up until we made a reference to an
    -- uninitialized variable
    , invalidErrMsgs :: [Text]
    -- ^ Messages explaining what went wrong
    }
  deriving (Show, Eq)

instance Pretty [InvalidStackTrace] where
  ppr []
    = "No errors"
  ppr errs@(_:_)
    = vcat . map ppr . List.nub $ errs

instance Pretty InvalidStackTrace where
  ppr (InvalidStackTrace [] msgs)
    = vcat
    $ map token msgs
  ppr (InvalidStackTrace strace msgs)
    = vcat
    $ map token (msgs ++ ["", "Stack trace leading up to error:"])
    ++ map ppr (reverse strace)

instance Pretty StackTraceItem where
  ppr (StackTraceItem m src dst)
    = "  - " <> ppr src <> ":" <> ppr m <+> "to" <+> ppr dst

instance Pretty StackTrace where
  ppr = vcat . map ppr

-- | Valid stack trace
data ValidStackTrace
  = ValidStackTrace
    { validStackTrace :: StackTrace
    -- ^ Path from "initial" state up until here
    , validEnv :: UndefinednessEnv
    -- ^ Mapping from variable name to initialized status. We expect
    -- every variable to be either initialized or unitialized: none
    -- are in an error state.
    }
  deriving Show

-------------------------------------------------------------------------------
-- Stack trace, expression and statement validation
-------------------------------------------------------------------------------

-- | Pretty print a location, given some context.
showAtLoc :: Loc -> Text -> Text
showAtLoc NoLoc x
  = x
showAtLoc l@Loc{} x
  = x <> " at " <> showLoc l

-- | Pretty print a location.
showLoc :: Loc -> Text
showLoc NoLoc = "<<no location info available>>"
showLoc (Loc line col) = "line " <> show line <> ":" <> show col

-- | If we reference a variable that is not in the map, it is a
-- temporary variable or a method argument, so we can assume it is
-- initialized (otherwise the expression would not be well-scoped,
-- which is currently caught by the type checker).  Otherwise, if it
-- is in the map and undefined, we "upgrade" the variable's value to
-- the error state.
checkVariable :: Located Name -> UndefinednessEnv -> UndefinednessEnv
checkVariable (Located loc var) env
  = case Map.lookup var env of
      Nothing
        -> env
      Just Initialized
        -> env
      Just Uninitialized
        -> Map.insert var (Error $ Set.singleton loc) env
      Just (Error err)
        -> Map.insert var (Error $ Set.insert loc err) env

-- | Check whether an "expression" refers to an uninitialized
-- variable. Returns an error whenever you feed it sequences or
-- assignments.
checkExpression
  :: LExpr -- ^ *expression* to check
  -> UndefinednessEnv
  -> Either Text UndefinednessEnv
checkExpression (Located loc (ESeq _ _)) _
  = Left (showAtLoc loc (showAtLoc loc "expected expression"))
checkExpression (Located _ (ELit _)) env
  = pure env
checkExpression (Located _ (EVar v)) env
  = let newEnv = checkVariable v env
    in pure newEnv
checkExpression (Located _ (EBinOp _ s0 s1)) env
  = envMeet <$> checkExpression s0 env <*> checkExpression s1 env
checkExpression (Located _ (EUnOp _ s)) env
  = checkExpression s env
checkExpression (Located _ (EIf c s0 s1)) env
  = envMeets <$> mapM (`checkExpression` env) [c, s0, s1]
checkExpression (Located _ (ECase s ms)) env
  = envMeets <$> mapM (`checkExpression` env) (s : map matchBody ms)
checkExpression (Located loc EBefore{}) _
  = Left (showAtLoc loc "expected expression")
checkExpression (Located loc EAfter{}) _
  = Left (showAtLoc loc "expected expression")
checkExpression (Located loc EBetween{}) _
  = Left (showAtLoc loc "expected expression")
checkExpression (Located loc EAssign{}) _
  = Left (showAtLoc loc "expected expression")
checkExpression (Located _ (ECall _ ss)) env
  = envMeets <$> mapM (`checkExpression` env) ss
checkExpression (Located _ ENoOp) env
  = pure env

-- | Collect all variables in an *expression*. Returns an error
-- whenever you feed it sequences or assignments.
expressionVars
  :: LExpr -- ^ *expression* to check
  -> Either Text (Set Name)
expressionVars (Located loc (ESeq _ _))
  = Left (showAtLoc loc "expected expression")
expressionVars (Located _ (ELit _))
  = pure Set.empty
expressionVars (Located _ (EVar v))
  = pure $ Set.singleton (locVal v)
expressionVars (Located _ (EBinOp _ s0 s1))
  = Set.union <$> expressionVars s0 <*> expressionVars s1
expressionVars (Located _ (EUnOp _ s))
  = expressionVars s
expressionVars (Located _ (EIf c s0 s1))
  = Set.unions <$> mapM expressionVars [c, s0, s1]
expressionVars (Located _ (ECase s ms))
  = Set.unions <$> mapM expressionVars (s : map matchBody ms)
expressionVars (Located loc EBefore{})
  = Left (showAtLoc loc "expected expression")
expressionVars (Located loc EAfter{})
  = Left (showAtLoc loc "expected expression")
expressionVars (Located loc EBetween{})
  = Left (showAtLoc loc "expected expression")
expressionVars (Located loc EAssign{})
  = Left (showAtLoc loc "expected expression")
expressionVars (Located _ (ECall _ ss)) -- XXX this should really be
                                         -- distinguishing between
                                         -- those primops that are
                                         -- safe and those that are
                                         -- not
  = Set.unions <$> mapM expressionVars ss
expressionVars (Located _ ENoOp)
  = pure Set.empty

-- | Check a statement. Returns an error whenever you feed it a naked
-- expression. We expect the input to be an assignment, primop call of
-- if-*statement* (and variants thereof).
checkStatement
  :: LExpr -- ^ *statement* to check
  -> (GraphLabel, UndefinednessEnv) -- ^ initial environment
  -> Either Text [(GraphLabel, UndefinednessEnv)] -- ^ mapping from
                                                  -- destination to
                                                  -- new environment
checkStatement (Located _ (ESeq s0 s1)) env
  = concat <$> (mapM (checkStatement s1) =<< checkStatement s0 env)
checkStatement (Located loc actual@(ELit _)) _
  = Left (showAtLoc loc $ "expected statement, got: " <> show actual)
checkStatement (Located loc actual@(EVar _)) _
  = Left (showAtLoc loc $ "expected statement, got: " <> show actual)
checkStatement (Located loc actual@EBinOp{}) _
  = Left (showAtLoc loc $ "expected statement, got: " <> show actual)
checkStatement (Located loc actual@(EUnOp _ _)) _
  = Left (showAtLoc loc $ "expected statement, got: " <> show actual)
checkStatement (Located _ (EIf c s0 s1)) (currState, env)
  = do
  condEnv <- checkExpression c env
  trueVals <- checkStatement s0 (currState, envMeet condEnv env)
  falseVals <- checkStatement s1 (currState, envMeet condEnv env)
  pure (trueVals ++ falseVals)
checkStatement (Located _ (ECase s ms)) (currState, env)
  = do
  scrutEnv <- checkExpression s env
  let newEnv = scrutEnv `envMeet` env
  concat <$> mapM (\m -> checkStatement (matchBody m) (currState, newEnv)) ms
checkStatement (Located _ (EBefore c s)) (currState, env)
  = do
  condEnv <- checkExpression c env
  checkStatement s (currState, envMeet condEnv env)
checkStatement (Located _ (EAfter c s)) (currState, env)
  = do
  condEnv <- checkExpression c env
  checkStatement s (currState, envMeet condEnv env)
checkStatement (Located _ (EBetween c0 c1 s)) (currState, env)
  = do
  cond0Env <- checkExpression c0 env
  cond1Env <- checkExpression c1 env
  let newEnv = envMeets [cond0Env, cond1Env, env]
  checkStatement s (currState, newEnv)
checkStatement (Located loc (EAssign var rhs)) (currState, oldEnv)
  = do
  newEnv <- checkAssignment loc oldEnv var rhs
  pure [(currState, newEnv)]
checkStatement (Located _ (ECall efunc ss)) (currState, oldEnv)
  = case efunc of
      -- Currently, helper functions are not allowed to have effects
      -- In the future, we will have to lookup the helper function body
      -- and call 'checkStatement' on it. However, we still check all the
      -- argument expressions to the helper function for undefinedness.
      Right nm ->
        pure . (currState,) . envMeets
          <$> mapM (`checkExpression` oldEnv) ss
      Left Prim.Terminate ->
        pure [(currState, oldEnv), (Main "terminal", oldEnv)]
      Left Prim.Transition ->
        case ss of
          [Located _ (ELit (Located _ (LState newState)))]
            -> pure [(currState, oldEnv), (Main newState, oldEnv)]
          _ -> Left "malformed primop args"
      Left _ ->
        pure . (currState,) . envMeets
          <$> mapM (`checkExpression` oldEnv) ss
checkStatement (Located _ ENoOp) env
  = pure [env]

-- | Check an assignment
checkAssignment
  :: Loc
  -> UndefinednessEnv
  -> Name
  -> LExpr
  -> Either Text UndefinednessEnv
checkAssignment loc oldEnv var rhs = do
  envRhs <- checkExpression rhs oldEnv
  varsRhs <- expressionVars rhs
  let newVal
        = replaceError
        . Map.foldr meet Initialized
        . Map.restrictKeys envRhs
        $ varsRhs
      newEnv
        = initializeInEnv var newVal (envMeet envRhs oldEnv)
      replaceError (Error _) = Error $ Set.singleton loc
      replaceError x = x

  pure newEnv

-- | Mapping from method names to expressions
type Methods = Map Name Method

-- | Map method names to their bodies (statements/expressions)
fetchScriptMethods :: Script -> Methods
fetchScriptMethods
  = Map.fromList
    . map (methodName &&& identity)
    . scriptMethods

-- | Validate a single stack trace
validateStackTrace
  :: UndefinednessEnv -- ^ initial environment, should only contain
                      -- globals/locals, i.e. not arguments/temps.
  -> Methods -- ^ map of method names and their bodies
  -> StackTrace -- ^ the stack trace to validate
  -> UndefinednessResult
validateStackTrace initEnv methods
  = foldl step emptyResult . getRelevantMethods
    where
      emptyResult :: UndefinednessResult
      emptyResult = Right $ ValidStackTrace [] initEnv

      step
        :: UndefinednessResult
        -> (Name, GraphLabel, GraphLabel, Method)
        -> UndefinednessResult
      step (Left invalidStackTrace) _
        = Left invalidStackTrace
      step (Right (ValidStackTrace strace oldEnv)) (m, src, dst, method)
        = case accessRestrictionErrs of
            errs@(_:_) ->
              Left $ InvalidStackTrace straceNew (errMsg errs)
            [] ->
              case newEnv of
                Left err
                  -> Left $ InvalidStackTrace straceNew (errMsg [err])
                Right env'
                  -> case collectErrors env' of
                       []
                         -> Right $ ValidStackTrace straceNew env'
                       errs@(_:_)
                         -> Left $ InvalidStackTrace straceNew (errMsg errs)

        where
          straceNew = extendStackTrace m src dst strace
          newEnv = (envMeets
                   . map snd
                   . filter ((== dst) . fst))
                   <$> checkStatement (methodBody method) (src, oldEnv)

          errMsg = ("In method " <> unName m <> ":" :)

          accessRestrictionErrs
            = case methodAccess method of
                RoleAny ->
                  []
                RoleAnyOf rs ->
                  -- checkExpression returns a Left case if the expression is a
                  -- statement, but this should be ruled out by type checking
                  assert ([] == (lefts . map (`checkExpression` oldEnv) $ rs))
                    $ collectErrors
                    . envMeets
                    . rights
                    . map (`checkExpression` oldEnv)
                    $ rs

      extendStackTrace
        :: Name -> GraphLabel -> GraphLabel -> StackTrace -> StackTrace
      extendStackTrace m src dst strace
        = strace ++ [StackTraceItem m src dst]

      getRelevantMethods
        :: StackTrace -> [(Name, GraphLabel, GraphLabel, Method)]
      getRelevantMethods
        = Maybe.mapMaybe lookupMethod

      lookupMethod
        :: StackTraceItem -> Maybe (Name, GraphLabel, GraphLabel, Method)
      lookupMethod (StackTraceItem m src dst) = do
        method <- Map.lookup m methods
        pure (m, src, dst, method)

-- | Collect erroneous variables from an environment
collectErrors
  :: UndefinednessEnv -> [Text]
collectErrors env'
  = concat [ displayError v e | (v, Error e) <- Map.toList env' ]
    where
      displayError v e
        = "Variable " <> show (unName v) <> " undefined at:"
          : map (("  - " <>) . showLoc) (Set.toList e)

-------------------------------------------------------------------------------
-- Generating stack traces
-------------------------------------------------------------------------------

-- | We keep track of how often an edge (method) is visited: 0 times
-- or >= 1 times.
data IsVisited
  = NeverVisited
  | Visited

-- | Increase the visit counter
addVisited :: IsVisited -> IsVisited
addVisited NeverVisited = Visited
addVisited Visited = Visited

-- | We generate stack traces by starting at the initial node and
-- adding whichever methods are reachable from that node. We
-- recursively keep on doing this until there are no more reachable
-- methods.
--
-- In order to deal with cycles in the graph, we keep track of how
-- often an edge (method) is visited. We make sure to only visit a
-- method once within a stack trace. (A method may of course be
-- visited multiple times across different stack traces.)
generateStackTraces :: Script -> [StackTrace]
generateStackTraces script
  = go initialVisited (Main "initial")
  where
    adjList :: [(Name, GraphLabel, GraphLabel)]
    adjList
      = map (\(n, src, dst) -> (n, Main src, Main dst))
        . GraphAnalysis.actualTransitions
        $ script

    adjMap :: Map GraphLabel [(Name, GraphLabel)]
    adjMap
      = foldr (\(m, src, dst) rest
                 -> Map.insertWith (++) src [(m, dst)] rest)
        Map.empty
        adjList

    initialVisited :: Map Name IsVisited
    initialVisited
      = Map.fromList
        . map (\(m, _, _) -> (m, NeverVisited))
        $ adjList

    go
      :: Map Name IsVisited
      -> GraphLabel
      -> [StackTrace]
    go visited src
      = case Map.lookup src adjMap of
          -- Implicitly lacking in outgoing edges
          Nothing
            -> [[]]
          -- Explicitly lacking in outgoing edges
          Just []
            -> [[]]
          -- > 0 outgoing edges
          Just ns@(_:_)
            -> concatMap (\(m, dst)
                            -> case Map.lookup m visited of
                                 Nothing
                                   -> [[]] -- The path ends here
                                 Just Visited
                                   -> [[]] -- We are going around in circles
                                 Just NeverVisited
                                   -> map (StackTraceItem m src dst :)
                                          (go (newVisited m) dst))
                         ns
        where
          newVisited m = Map.adjust addVisited m visited
