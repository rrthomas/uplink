{-# LANGUAGE TupleSections #-}

module Script.Init (
  createContract,
  createContractWithEvalCtx,

  createFauxContract,
) where

import Protolude
import qualified Data.Map as Map

import Script
import Address (Address, AAccount, AContract)
import Contract (Contract)

import Key (PrivateKey)
import Time (Timestamp)
import qualified Encoding
import qualified Contract
import qualified SafeString
import qualified Hash
import qualified Transaction as TX
import qualified Script.Graph as Graph
import qualified Script.Storage as Storage
import qualified Script.Compile as Compile
import Ledger (World)
import Script.Eval (EvalCtx(..))
import qualified Homomorphic as Homo

-- | Create a contract
createContract
  :: Address AContract                    -- ^ Contract Address
  -> Int64                                -- ^ Current Block Index
  -> Timestamp                            -- ^ Current Block Timestamp
  -> Address AAccount                     -- ^ Address of Evaluating node
  -> Hash.Hash Encoding.Base16ByteString  -- ^ Current Transaction hash
  -> Address AAccount                     -- ^ Issuer of transaction (tx origin field)
  -> PrivateKey                           -- ^ Node private key for signing
  -> Timestamp                            -- ^ Contract timestamp
  -> Address AAccount                     -- ^ Contract owner
  -> World                                -- ^ Initial world
  -> Text                                 -- ^ Raw FCL code
  -> IO (Either Text Contract)
createContract contractAddr blockIdx blockTs nodeAddr txHash txOrigin privKey cTimestamp cOwner world body = do
  (pub,_) <- Homo.genRSAKeyPair Homo.rsaKeySize -- XXX: Actual key of validator
  let evalCtx = EvalCtx
        { currentBlock = blockIdx
        , currentValidator = nodeAddr
        , currentTransaction = txHash
        , currentTimestamp = blockTs
        , currentCreated = cTimestamp
        , currentDeployer = cOwner
        , currentTxIssuer = txOrigin
        , currentAddress = contractAddr
        , currentPrivKey = privKey
        , currentStorageKey = pub
        }
  createContractWithEvalCtx evalCtx world body

-- | Create a contract with a supplied evaluation context
createContractWithEvalCtx
  :: EvalCtx    -- ^ Context to evaluate the top-level definitions in
  -> World      -- ^ Initial world
  -> Text       -- ^ Raw FCL code
  -> IO (Either Text Contract)
createContractWithEvalCtx evalCtx world body
  = case Compile.compile body of
      Left err
        -> pure (Left err)
      Right (_, ast)
        -> do
        gs <- Storage.initStorage evalCtx world ast
        pure . pure $ Contract.Contract
               { Contract.timestamp        = currentTimestamp evalCtx
               , Contract.script           = ast
               , Contract.globalStorage    = gs
               , Contract.localStorage     = Map.empty
               , Contract.localStorageVars = Storage.initLocalStorageVars ast
               , Contract.methods          = Script.methodNames ast
               , Contract.state            = Graph.GraphInitial
               , Contract.owner            = currentDeployer evalCtx
               , Contract.address          = currentAddress evalCtx
               }

-- | This function is used to create Contracts that explicitly _won't_ be
-- submitted to the ledger. This function creates a transaction and then derives
-- an address from the transaction. Notably, this address is _not_ guaranteed to
-- be unique, because the transaction will NOT be submitted to the network for
-- uniqueness verification. This is to be used in places like the REPL and the
-- Simulation process.
createFauxContract
  :: Int64                                -- ^ Current Block Index
  -> Timestamp                            -- ^ Current Block Timestamp
  -> Address AAccount                     -- ^ Address of Evaluating node
  -> Hash.Hash Encoding.Base16ByteString  -- ^ Current Transaction hash
  -> Address AAccount                     -- ^ Issuer of transaction (tx origin field)
  -> PrivateKey                           -- ^ Node private key for signing
  -> Timestamp                            -- ^ Contract timestamp
  -> Address AAccount                     -- ^ Contract owner
  -> World                                -- ^ Initial world
  -> Text                                 -- ^ Raw FCL code
  -> IO (Either Text Contract)
createFauxContract blockIdx blockTs nodeAddr txHash txOrigin privKey cTimestamp cOwner world body = do
  let contractHdr = TX.TxContract $ TX.CreateContract (SafeString.fromBytes' $ toS body)
  contractAddr <- TX.transactionToAddress <$> TX.newTransaction nodeAddr privKey contractHdr
  createContract contractAddr blockIdx blockTs nodeAddr txHash txOrigin privKey cTimestamp cOwner world body
