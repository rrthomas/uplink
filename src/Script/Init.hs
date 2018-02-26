{-# LANGUAGE TupleSections #-}

module Script.Init (
  initLocalStorageVars,
  createContract,
  createContractWithAddr,
) where

import Protolude
import qualified Data.Map as Map
import qualified Data.Set as Set

import Script
import Address (Address, fromRaw)
import Contract (Contract)
import Encoding (b58)

import qualified Time
import qualified Script
import qualified Storage
import qualified Contract
import qualified Hash
import qualified Script.Graph as Graph
import qualified Script.Storage as Storage
import qualified Script.Compile as Compile

-- | Create a contract and derive an address in the process
createContract
  :: Address         -- ^ Owner Address
  -> Time.Timestamp  -- ^ Timestamp of creation
  -> Text            -- ^ Raw FCL code
  -> Either Text Contract
createContract ownerAddr ts body = do
    createContractWithAddr ownerAddr contractAddr ts body
  where
    contractAddr =
      fromRaw $ b58 $ Hash.sha256Raw $
        mconcat $ map Hash.getHash
          [ Hash.toHash ownerAddr, Hash.toHash ts , Hash.toHash body ]

-- | Create a contract with a supplied address
createContractWithAddr
  :: Address         -- ^ Owner Address
  -> Address         -- ^ Address of Contract
  -> Time.Timestamp  -- ^ Timestamp of creation
  -> Text            -- ^ Raw FCL code
  -> Either Text Contract
createContractWithAddr ownerAddr contractAddr ts body = do
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
initLocalStorageVars (Script _ defns _ _) =
  Contract.LocalStorageVars $ Set.fromList
    [ name | LocalDefNull _ (Located _ name) <- defns ]
