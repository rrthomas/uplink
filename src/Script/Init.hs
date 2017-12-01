{-# LANGUAGE TupleSections #-}

module Script.Init (
  initLocalStorageVars,
  createContract,
) where

import Protolude
import qualified Data.Map as Map
import qualified Data.Set as Set

import Script
import Address (Address)
import Contract (Contract)

import qualified Time
import qualified Script
import qualified Storage
import qualified Contract
import qualified Script.Graph as Graph
import qualified Script.Storage as Storage
import qualified Script.Compile as Compile

createContract
   :: Address         -- ^ Owner Address
   -> Address         -- ^ Address of Contract
   -> Time.Timestamp  -- ^ Timestamp of creation
   -> Text            -- ^ Raw func code
   -> Either Text Contract
createContract ownerAddr contractAddr ts body = do
  (sigs, ast) <- Compile.compile body
  pure $ Contract.Contract {
      Contract.timestamp        = ts
    , Contract.script           = ast
    , Contract.globalStorage    = Storage.initStorage ast
    , Contract.localStorage     = Map.empty
    , Contract.localStorageVars = initLocalStorageVars ast
    , Contract.methods          = Script.methodNames ast
    , Contract.state            = Graph.GraphInitial
    , Contract.owner            = ownerAddr
    , Contract.address          = contractAddr
    }

initLocalStorageVars :: Script -> Contract.LocalStorageVars
initLocalStorageVars (Script defns _ _) =
  Contract.LocalStorageVars $ Set.fromList
    [ name | LocalDefNull _ (Located _ name) <- defns ]
