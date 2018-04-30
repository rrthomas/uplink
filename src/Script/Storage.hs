{-|

Memory state for FCL execution.

--}

{-# LANGUAGE TupleSections #-}

module Script.Storage (
  initLocalStorageVars,
  initStorage,
  dumpStorage,
) where

import Protolude hiding ((<>))

import Ledger (World)
import Address
import Script
import Storage
import SafeInteger
import Script.Pretty
import Script.Eval (EvalM, EvalState(..), EvalCtx(..))
import qualified Script.Eval as Eval
import qualified Contract
import qualified Data.Map as Map
import qualified Data.Set as Set
import Script.Graph (GraphState(..))

import Control.Monad.State.Strict (modify')

initLocalStorageVars :: Script -> Contract.LocalStorageVars
initLocalStorageVars (Script _ defns _ _) =
  Contract.LocalStorageVars $ Set.fromList
    [ name | LocalDefNull _ (Located _ name) <- defns ]

initGlobalStorage :: Script -> Storage
initGlobalStorage (Script _ defns _ _)
  = foldl' buildStores mempty defns
  where
    buildStores :: Storage -> Def -> Storage
    buildStores gstore = \case
      LocalDef TInt (Name nm) lit ->
        Map.insert (Key nm) VUndefined gstore

      GlobalDef type_ (Name nm) expr ->
        Map.insert (Key nm) VUndefined gstore

      GlobalDefNull _ (Located _ (Name nm)) ->
        Map.insert (Key nm) VUndefined gstore

      -- XXX what are we supposed to do with local vars?
      _ -> gstore

initStorage
  :: EvalCtx    -- ^ Context to evaluate the top-level definitions in
  -> World      -- ^ World to evaluate the top-level definitions in
  -> Script     -- ^ Script
  -> IO GlobalStorage
initStorage evalCtx world s@(Script _ defns _ _)
  = do
  res <- Eval.execEvalM evalCtx emptyEvalState $ mapM_ assignGlobal defns
  case res of
    Left err -> die $ show err
    Right state -> pure . GlobalStorage . globalStorage $ state
  where
    assignGlobal :: Def -> EvalM ()
    assignGlobal = \case
      LocalDef type_ nm expr -> do
        val <- Eval.evalLExpr expr
        modify' (insertVar nm (toVCrypto TInt val))
      GlobalDef type_ nm expr -> do
        val <- Eval.evalLExpr expr
        modify' (insertVar nm val)
      _ -> pure ()

    insertVar (Name nm) val st
      = st { globalStorage =
               Map.insert (Key nm) val (globalStorage st)
           }

    emptyEvalState :: EvalState
    emptyEvalState = EvalState
      { tempStorage      = mempty
      , globalStorage    = initGlobalStorage s
      , localStorage     = mempty
      , localStorageVars = initLocalStorageVars s
      , graphState       = GraphInitial
      , sideState        = Nothing
      , sideLock         = (False, Nothing)
      , worldState       = world
      , deltas           = []
      }

toVCrypto :: Script.Type -> Value -> Value
toVCrypto TInt (VInt n) = VCrypto $ toSafeInteger' n
toVCrypto t     v       = convAddr t v

convAddr :: Script.Type -> Value -> Value
convAddr TAccount (VAddress addr) = VAccount (addrFromUnknown addr)
convAddr (TAsset _)(VAddress addr) = VAsset (addrFromUnknown addr)
convAddr TContract (VAddress addr) = VContract (addrFromUnknown addr)
convAddr _ x = x

-- | Pretty print storage map
dumpStorage :: EnumInfo -> Map Key Value -> Doc
dumpStorage enumInfo store =
  if Map.null store
    then indent 8 "<empty>"
    else
      indent 8 $
      vcat [
          ppr k                         -- variable
          <+> ":" <+> pprTy v  -- type
          <+> "=" <+> ppr v             -- value
          | (k,v) <- Map.toList store
        ]
  where
    pprTy v = case mapType enumInfo v of
                Nothing -> "<<unknown constructor>>"
                Just ty -> ppr ty
