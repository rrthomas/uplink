{-|

Script graph analysis passes.

-}

{-# LANGUAGE Strict #-}

module Script.Analysis (
  -- ** Branch analysis
  GraphTransitions,
  branches,
  subbranches,
  checkGraph,

  -- ** FGL
  CFG,
  makeGraph,
  dominators,
  choices,

  -- ** Transitions
  transitions,
  transitionLabels,

  -- ** Definition
  definitions,

  -- ** Error reporting
  GraphError(..),
  prettyError,

  -- ** Testing
  graphviz,
  testVis,
) where

import Protolude
import Unsafe

import Script
import Script.Prim
import Script.Graph
import Script.Pretty hiding ((<>))

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

import System.Process (callCommand)

import qualified Data.Graph.Inductive as Graph
import qualified Data.Graph.Inductive.Query.ArtPoint as Graph
import qualified Data.Graph.Inductive.Query.Dominators as Graph

-------------------------------------------------------------------------------
-- Branch Analysis
-------------------------------------------------------------------------------

branchesMethod :: Method -> [Label]
branchesMethod = fmap toLabel . concatMap go . unseq . methodBody
  where
    go :: LExpr -> [LLit]
    go (Located l (ECall prim args))
      | prim == (primName Transition) = argLits (fmap unLoc args)
      | prim == (primName Terminate) = [LState "terminal" `at` l]
      | otherwise = []
    go _ = []

    -- Safe only if run on typechecked programs.
    toLabel :: LLit -> Label
    toLabel (Located l (LState label)) = label
    toLabel _ = panic "Malformed program."

-- | Extract the possible branch statements in the AST. Only valid for
-- type-safe programs.
branches :: Script -> [ (Name, Label, [Label]) ]
branches (Script defs trans methods) =
  [(methodName method, unGLabel (methodTag method), branchesMethod method)
    | method <- methods, isMainGraph (methodTag method)]

subbranches :: Script -> [ (Name, Label, [Label]) ]
subbranches (Script defs trans methods) =
  [(methodName method, unGLabel (methodTag method), branchesMethod method)
    | method <- methods, isSubGraph (methodTag method)]

unGLabel :: GraphLabel -> Label
unGLabel (Main x) = x
unGLabel (Subg x) = x

isMainGraph :: GraphLabel -> Bool
isMainGraph (Main x) = True
isMainGraph _ = False

isSubGraph :: GraphLabel -> Bool
isSubGraph = not . isMainGraph

-------------------------------------------------------------------------------
-- Error Reporting
-------------------------------------------------------------------------------

data GraphError
  = CannotTransition Label Label
  | Unreachable Label
  | NoEntry
  deriving (Eq, Ord, Show)

instance Pretty GraphError where
  ppr = \case
    CannotTransition from to -> "Cannot transition from state: " <> squotes (ppr from) <> " to " <> squotes (ppr to)
    Unreachable label        -> "Cannot reach state "  <> squotes (ppr label) <> " in graph."
    NoEntry                  -> "Cannot enter graph state, no valid entry points from 'initial'"

prettyError :: GraphError -> Text
prettyError = Script.Pretty.prettyPrint

-------------------------------------------------------------------------------
-- Transition Analysis
-------------------------------------------------------------------------------

type GraphTransitions = [(Label, Label)]
type GraphBranches = [(Name, Label, [Label])]

isAllowed :: [Transition] -> Label -> Label -> Bool
isAllowed xs from to = and (fmap (valid from to) xs)

valid :: Label -> Label -> Transition -> Bool
valid from' to' (Arrow (Step from) (Step to)) = (from == from') && (to == to')
valid _ _ _ = False

toLabel :: Transition -> [(Label, Label)]
toLabel = \case
  Arrow (Step from) (Step to) -> [(from, to)]
  Arrow Initial (Step a)      -> [(initialLabel, a)]
  Arrow (Step a) Terminal     -> [(a, terminalLabel)]
  Arrow Initial Terminal      -> [(initialLabel, terminalLabel)]
  _                           -> []

transitions :: Script -> [(Label, Label)]
transitions script = concatMap toLabel (scriptTransitions script)

isConsistent
  :: [(Label, Label)]            -- ^ Transitions
  -> GraphBranches               -- ^ Branches
  -> Either GraphError GraphTransitions
isConsistent xs ys =
  if and [Set.member x allowed | x <- branches]
    then Right xs
    else Left (inconsistent [x | x <- branches, not (Set.member x allowed) ])
  where
    branches = [(from, to) | (a,from,tos) <- ys, to <- tos]
    allowed  = Set.fromList xs
    entry    = entryPoints

-- XXX: show all violations instead of just the first
inconsistent :: [(Label, Label)] -> GraphError
inconsistent ((from,to):xs) = CannotTransition from to
inconsistent [] = panic "Impossible"

-- | Check for inconsistent transition states.
checkGraph :: Script -> Either GraphError GraphTransitions
checkGraph script
  | not (hasEntry script) = Left NoEntry
  | otherwise             =  do
      isConsistent (transitions script) (branches script)
      isConsistent (transitions script) (subbranches script)

-- | Check for existence of entry points to the graph.
hasEntry :: Script -> Bool
hasEntry script = length (entryPoints script) > 0

-- | Get the entry points to the graph, transitions from
-- 'initial' lablel.
entryPoints :: Script -> [(Label, Label)]
entryPoints script = [(from, to) | (from, to) <- (transitions script), from == initialLabel]

-- | Check for unreachable transition states.
checkUnreachable :: Script -> Either GraphError GraphTransitions
checkUnreachable script = Right (transitions script)
  where
    reached = Set.fromList (transitionLabels script)

-------------------------------------------------------------------------------
-- Visualization
-------------------------------------------------------------------------------

-- XXX: very very inefficient and unsafe

type CFG = Graph.Gr Label Int

-- | All nodes
transitionLabels :: Script -> [Label]
transitionLabels script = ordNub $ concatMap go (transitions script)
  where
    go (a,b) = [a,b]

-- | Construct the fgl interface for the nodes/edges of the specified
-- transitions and branch points.
makeGraph :: Script -> CFG
makeGraph script = Graph.mkGraph nodes edges'
  where
    nodes = zip [1..] (transitionLabels script)
    edges = [(from, to) | (a,from,tos) <- branches script, to <- tos] -- ++ entryPoints script

    nodeMap = Map.fromList (fmap swap nodes)

    edges' = [(nodeIx from, nodeIx to, i) | (i, (from, to)) <- zip [1..] edges]
      where
        nodeIx x = nodeMap Map.! x

dominators :: CFG -> Int -> [(Graph.Node, [Graph.Node])]
dominators = Graph.dom

choices :: CFG -> [Graph.Node]
choices = Graph.ap

-------------------------------------------------------------------------------
-- Defnition Analysis
-------------------------------------------------------------------------------

definitionsMethod :: Method -> [Name]
definitionsMethod = concatMap go . unseq . methodBody
  where
    go (Located l (EAssign n args)) = [n]
    go _ = []

-- | Extract all assignment references inside of a block.
definitions :: Script -> [ (Name, [Name]) ]
definitions (Script defs trans methods) =
  [(methodName method, definitionsMethod method) | method <- methods]

-------------------------------------------------------------------------------
-- Visualization
-------------------------------------------------------------------------------

graphviz :: (Graph.Graph g, Show a, Show b)
  => g a b   -- ^ The graph to format
  -> [Char]  -- ^ The title of the graph
  -> [Char]
graphviz g t =
  let n = Graph.labNodes g
      e = Graph.labEdges g
      ns = concatMap sn n
      es = concatMap se e
  in "digraph "++t++" {\n"
          ++ ns
          ++ es
      ++"}"
  where sn (n, a) | sa == ""  = ""
                  | otherwise = '\t':(show n ++ sa ++ "\n")
          where sa = sl a
        se (n1, n2, b) = '\t':(show n1 ++ " -> " ++ show n2 ++ sl b ++ "\n")

sl :: (Show a) => a -> [Char]
sl a = let l = sq (show a)
  in if (l /= "()") then (" [label = " ++ show l ++ "]") else ""

sq :: [Char] -> [Char]
sq s@[_] = s
sq ('"':s)
  | unsafeLast s == '"'  = unsafeInit s
  | otherwise = s
sq ('\'':s)
  | unsafeLast s == '\'' = unsafeInit s
  | otherwise = s
sq s = s

testVis :: (Show a, Show b) => Graph.Gr a b -> IO ()
testVis gr = do
  let dat = graphviz gr "foo"
  writeFile "output.dot" (toS dat)
  rc <- callCommand $ "circo -Tpng output.dot -o output.png"
  pure ()
