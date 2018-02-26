{-|

Script graph analysis passes. Checks for the following:

 * Validity of transitions: do we transitionTo a state in accordance
   to a transition declared earlier?

 * Completeness of transitions: do we transitionTo all states we have
   declarations for?

 * Reachability of the terminal state from any state.

 * Reachability of any state from the initial state.

-}

{-# LANGUAGE Strict #-}

module Script.Analysis (
  -- * Run all graph analyses and report on errors
  checkGraph
  -- * Retrieve graph structure info from scripts
  , actualTransitions
  , validTransitions
  , branches
  , choices
  , makeGraph
  -- * Graph analyses
  , completenessViolations
  , soundnessViolations
  , reachability
  -- * Visualise the graph analyses' results
  , graphviz
  , testVis
  ) where

import Protolude

import Script
import Script.Pretty hiding ((<>))
import qualified Script.Graph as ScrGraph
import qualified Script.Prim as Prim

import Data.Bifunctor (bimap)
import qualified Data.Graph.Inductive as FGL
import qualified Data.List as List
import qualified Data.Tuple as Tuple
import qualified Data.Text as Text
import qualified Data.Either as Either

import qualified System.Process as Process

-------------------------------------------------------------------------------
-- Run the analyses and report on the errors
-------------------------------------------------------------------------------

-- | Types of errors that may emerge from the graph checks.
data GraphError
  = MissingTransition Label Label
  | IllegalTransition Name Label Label
  | TerminalUnreachable Label
  | UnreachableFromInitial Label
  | TransitionToInitial Label
  | TransitionFromTerminal Label
  | NoEntry
  deriving (Eq, Ord, Show)

instance Pretty [GraphError] where
  ppr [] = "No graph errors"
  ppr errs@(_:_) = vcat ("We have the following graph errors:":map ppr errs)

instance Pretty GraphError where
  ppr = \case
    MissingTransition from to
      -> "Missing transition from state: "
         <> squotes (ppr from)
         <> " to "
         <> squotes (ppr to)
    IllegalTransition methodName from to
      -> "Cannot transition from state: "
         <> squotes (ppr from)
         <> " to "
         <> squotes (ppr to)
         <> " in method "
         <> squotes (ppr methodName)
         <> "."
    TerminalUnreachable label
      -> "Cannot reach terminal state from "
         <> squotes (ppr label)
         <> "."
    UnreachableFromInitial label
      -> "Cannot reach state "
         <> squotes (ppr label)
         <> " from initial state in graph."
    TransitionToInitial from
      -> "Illegal transition declaration: transition from "
         <> squotes (ppr from)
         <> " to initial state is not allowed."
    TransitionFromTerminal to
      -> "Illegal transition declaration: transition from terminal state to: "
         <> squotes (ppr to)
         <> " is not allowed."
    NoEntry
      -> "Cannot enter graph state, no valid entry points from 'initial'"

-- | Perform all analyses on the state graph and report all errors
-- found. If we do not encounter anything, we return the graph
checkGraph :: Script -> Either [GraphError] GraphTransitions
checkGraph scr = case allErrors of
                   [] -> Right valid
                   _ -> Left allErrors
  where
    allErrors
      = concat
        [ illegal
        , unsound
        , incomplete
        , notReachable
        , notReaching
        , noEntry
        ]

    actual = actualTransitions scr
    valid = Either.rights $ validTransitions scr

    illegal = Either.lefts $ validTransitions scr

    unsound
      = map (\(methodName, src, dest)
               -> IllegalTransition methodName src dest)
        $ soundnessViolations actual valid

    incomplete
      = map (uncurry MissingTransition)
        $ completenessViolations actual valid

    (notReachable, notReaching)
      = bimap (map UnreachableFromInitial)
              (map TerminalUnreachable)
        $ reachability scr

    noEntry
      = if hasEntry scr
        then []
        else [NoEntry]

-------------------------------------------------------------------------------
-- Implementation of the graph checks
-------------------------------------------------------------------------------

type GraphTransition = (Label, Label)
type GraphTransitions = [GraphTransition]

-- | Check for existence of entry points to the graph.
hasEntry :: Script -> Bool
hasEntry = any (\(from, _) -> from == ScrGraph.initialLabel)
           . Either.rights
           . validTransitions

-- | Fish out the transition declarations from the script.
validTransitions :: Script -> [Either GraphError GraphTransition]
validTransitions = map toLabel . scriptTransitions
  where
    toLabel :: Transition -> Either GraphError GraphTransition
    toLabel = \case
              Arrow (Step from) (Step to) -> Right (from, to)
              Arrow Initial (Step a)      -> Right (ScrGraph.initialLabel, a)
              Arrow (Step a) Terminal     -> Right (a, ScrGraph.terminalLabel)
              Arrow Initial Terminal      -> Right ( ScrGraph.initialLabel
                                                   , ScrGraph.terminalLabel
                                                   )
              Arrow (Step a) Initial      -> Left (TransitionToInitial a)
              Arrow Terminal (Step a)     -> Left (TransitionFromTerminal a)
              _                           -> panic "wut"

-- | Fish out the actual transitions, as we get them from methods and
-- their "transitionTo" statements, grouped by method name and source
-- state.
branches :: Script -> [ (Name, Label, [Label]) ]
branches (Script _ defs trans methods)
  = map extractBranch methods
  where
    extractBranch :: Method -> (Name, Label, [Label])
    extractBranch method = ( methodName method
                           , unGLabel (methodTag method)
                           , branchesMethod method
                           )

    branchesMethod :: Method -> [Label]
    branchesMethod = fmap toLabel . concatMap go . unseq . methodBody
      where
        go :: LExpr -> [LLit]
        go (Located l (ECall prim args))
          | prim == Prim.primName Prim.Transition = argLits (fmap unLoc args)
          | prim == Prim.primName Prim.Terminate = [LState "terminal" `at` l]
          | otherwise = []
        go _ = []

        -- Safe only if run on typechecked programs.
        toLabel :: LLit -> Label
        toLabel (Located l (LState label)) = label
        toLabel _ = panic "Malformed program."

    unGLabel :: GraphLabel -> Label
    unGLabel (Main x) = x
    unGLabel (Subg x) = x

-- | Fish out the transitions implied by the methods. As such, we
-- label each transition.
actualTransitions :: Script -> [(Name, Label, Label)]
actualTransitions = concatMap flattenBranch . branches
  where
    flattenBranch (n, src, dests) = map (\dest -> (n, src, dest)) dests

-- | All actual transitions are valid transitions. As it stands
-- currently, the type checker already checks for soundness
-- violations. This means that for well-typed scripts, that the
-- resulting list will be empty.
soundnessViolations
  :: [(Name, Label, Label)]
  -> GraphTransitions
  -> [(Name, Label, Label)]
soundnessViolations actual valid
  = filter (\(_,src,dst) -> (src,dst) `notElem` valid) actual

-- | All valid transitions have at least one actual transition as
-- their counterpart.
completenessViolations
  :: [(Name, Label, Label)]
  -> GraphTransitions
  -> GraphTransitions
completenessViolations actual valid
  = valid List.\\ map forgetName actual
  where
    forgetName (_,src,dst) = (src,dst)

-- | Retrieve all graph nodes where there is more than one outgoing
-- edge.
choices :: CFG -> [FGL.Node]
choices = FGL.ap

type CFG = FGL.Gr Label ()

-- | Create the graph from an FCL script that has passed the
-- type-checker.
makeGraph :: Script -> CFG
makeGraph scr = FGL.mkGraph stateNodes stateEdges
    where
      valid = Either.rights $ validTransitions scr

      stateGraph :: FGL.Gr Label ()
      stateGraph = FGL.mkGraph stateNodes stateEdges

      labels :: [Label]
      labels = List.nub . concatMap (\(src,dst) -> [src, dst]) $ valid

      stateNodes :: [FGL.LNode Label]
      stateNodes = zip [1..] labels

      stateEdges :: [FGL.LEdge ()]
      stateEdges = map (\(src,dst) -> (getIx src, getIx dst, ())) valid
          where
            getIx :: Label -> Int
            getIx lbl = fromMaybe (panic "getIx: impossible")
                                  (List.lookup lbl (map Tuple.swap stateNodes))

      forgetLabel :: FGL.LNode a -> FGL.Node
      forgetLabel = fst

-- | If we have performed the soundness and completeness checks, it
-- does not matter which of the two graphs we perform the reachability
-- analysis on. For simplicity its sake, we will go for the transition
-- declarations instead of the implied transitions.
--
-- We return a list of nodes which are not reachable from the initial
-- state and a list of nodes from which the terminal state is not
-- reachable.
reachability :: Script -> ([Label],[Label])
reachability scr = (notReachable, notReaching)
    where 
      valid = Either.rights $ validTransitions scr

      stateGraph = makeGraph scr

      labels :: [Label]
      labels = map snd stateNodes

      stateNodes :: [FGL.LNode Label]
      stateNodes = FGL.labNodes stateGraph

      forgetLabel :: FGL.LNode a -> FGL.Node
      forgetLabel = fst

      getLabel :: FGL.Node -> Label
      getLabel i
        = case map snd . filter ((== i) . fst) $ stateNodes of
            [] -> panic "getLabel: impossible"
            [l] -> l
            (_:_:_) -> panic "getLabel: impossible"

      notReachable :: [Label]
      notReachable
        = filter (/= ScrGraph.initialLabel)
          $ labels List.\\ map getLabel reachableFromInitial

      notReaching :: [Label]
      notReaching
        = filter (/= ScrGraph.terminalLabel)
          $ labels List.\\ map getLabel reachingToTerminal

      reachableFromInitial :: [FGL.Node]
      reachableFromInitial
          = case filter ((== ScrGraph.initialLabel) . snd) stateNodes of
              []
                -> map forgetLabel stateNodes
              [initialNode]
                -> FGL.suc (FGL.tc stateGraph) (forgetLabel initialNode)
              (_:_:_)
                -> panic "reachableFromInitial: impossible"
      -- More than one initial node, should be impossible.

      reachingToTerminal :: [FGL.Node]
      reachingToTerminal
          = case filter ((== ScrGraph.terminalLabel) . snd) stateNodes of
              []
                -> map forgetLabel stateNodes
              [initialNode]
                -> FGL.pre (FGL.tc stateGraph) (forgetLabel initialNode)
              (_:_:_)
                -> panic "reachingToTerminal: impossible"
      -- More than one terminal node, should be impossible.

-------------------------------------------------------------------------------
-- Visualisation of the graph analysis
-------------------------------------------------------------------------------

data NodeLabel = NotReachableLabel Label
               | NotReachingLabel Label
               | NodeLabel Label

data EdgeLabel = MissingEdge
               | IllegalEdge Name
               | LegalEdge Name

-- | Create a graph from a script with all the information to
-- visualise the reasons why the script might not have passed the
-- graph checks.
createGraph :: Script -> ([NodeLabel], [(EdgeLabel, Label, Label)])
createGraph scr = (allLabels, allEdges)
  where
    valid = Either.rights $ validTransitions scr
    actual = actualTransitions scr

    allLabels :: [NodeLabel]
    allLabels = map classifyLabel
                . List.nub
                $ validLabels ++ actualLabels
      where
        validLabels = concatMap (\(src,dst) -> [src,dst]) valid
        actualLabels = concatMap (\(_,src,dst) -> [src,dst]) actual
        classifyLabel l | l `elem` notReachable = NotReachableLabel l
                        | l `elem` notReaching = NotReachableLabel l
                        | otherwise = NodeLabel l

    allEdges :: [(EdgeLabel, Label, Label)]
    allEdges = map (\(n,src,dst) -> if (n,src,dst) `elem` unsound
                                    then (IllegalEdge n, src, dst)
                                    else (LegalEdge n, src, dst)
                   ) actual
               ++
               map (\(src,dst) -> (MissingEdge, src, dst)) missing
        where
          missing = completenessViolations actual valid
          unsound = soundnessViolations actual valid

    (notReachable, notReaching) = reachability scr

-- | Create a graphviz script from an FCL script and visualise the
-- reasons why it does not pass the graph check, if there are any.
graphviz
    :: Script
    -> Text
graphviz scr = Text.unlines
                $ [ "digraph state_machine {" ]
                ++ nodesTxt
                ++ edgesTxt
                ++ [ "}", "" ]
    where
      (nodes,edges) = createGraph scr

      nodesTxt :: [Text]
      nodesTxt = map (\case
                       NotReachingLabel l -> unLabel l <> " [ shape = doubleoctagon ]"
                       NotReachableLabel l -> unLabel l <> " [ shape = tripleoctagon ]"
                       NodeLabel "initial" -> "initial [ shape = point ]"
                       NodeLabel "terminal" -> "terminal [ shape = doublecircle ]"
                       NodeLabel l -> unLabel l
                     ) nodes

      edgesTxt :: [Text]
      edgesTxt = map (\(edgeLabel, src, dst) -> Text.unwords
                      [ unLabel src, "->", unLabel dst, edgeLabelTxt edgeLabel ]) edges

      edgeLabelTxt MissingEdge
          = "[ style = dotted ]"
      edgeLabelTxt (IllegalEdge l)
          = "[ label = " <> unName l <> ", style = dashed ]"
      edgeLabelTxt (LegalEdge l)
          ="[ label = " <> unName l <> " ]"

-- | Run "circo" on the graphviz output of an FCL script.
testVis :: Script -> IO ()
testVis scr = do
  let dat = graphviz scr
  writeFile "output.dot" dat
  Process.callCommand "circo -Tpng output.dot -o output.png"
