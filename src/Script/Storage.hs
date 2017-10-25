{-|

Memory state for FCL execution.

--}

{-# LANGUAGE TupleSections #-}

module Script.Storage (
  initStorage,
  dumpStorage,
) where

import Protolude hiding ((<>))

import Script
import Storage
import SafeInteger
import Script.Pretty

import qualified Data.Map as Map

initStorage :: Script -> GlobalStorage
initStorage (Script defns graph _) = GlobalStorage $
    foldl' buildStores Map.empty defns
  where
    buildStores :: Storage -> Def -> Storage
    buildStores gstore = \case
      GlobalDef type_ (Name nm) lit ->
        Map.insert (Key $ encodeUtf8 nm) (convAddr type_ $ evalLLit lit) gstore

      LocalDef TInt (Name nm) lit ->
        Map.insert (Key $ encodeUtf8 nm) (toVCrypto TInt $ evalLLit lit) gstore

      GlobalDefNull _ (Located _ (Name nm)) ->
        Map.insert (Key $ encodeUtf8 nm) VUndefined gstore

      _ -> gstore

    toVCrypto TInt (VInt n) = VCrypto $ toSafeInteger' n
    toVCrypto t     v       = convAddr t v

    convAddr TAccount (VAddress addr)  = VAccount addr
    convAddr TAsset (VAddress addr)    = VAsset addr
    convAddr TContract (VAddress addr) = VContract addr
    convAddr _          x              = x

-- | Pretty print storage map
dumpStorage :: Map Key Value -> Doc
dumpStorage store =
  if Map.null store
    then indent 8 "<empty>"
    else
      indent 8 $
      vcat [
          ppr k                         -- variable
          <+> ":" <+> ppr (mapType v)   -- type
          <+> "=" <+> ppr v             -- value
          | (k,v) <- Map.toList store
        ]
