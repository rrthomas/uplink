{-# LANGUAGE TupleSections #-}

module Script.Init (
  scriptToContract,
  createContract,
  createContractWithEvalCtx,
) where

import Protolude
import qualified Data.Map as Map
import qualified Data.Set as Set

import Script
import Address (Address, fromRaw, emptyAddr)
import Contract (Contract)
import Encoding (b58)

import Key (PrivateKey)
import Time (Timestamp)
import qualified Script
import qualified Storage
import qualified Contract
import qualified Hash
import qualified Script.Graph as Graph
import qualified Script.Storage as Storage
import qualified Script.Compile as Compile
import qualified Derivation
import Ledger (World)
import Script.Eval (EvalCtx(..))
import qualified Homomorphic as Homo

scriptToContract
  :: Int64      -- ^ Current Block Index
  -> Timestamp  -- ^ Current Block Timestamp
  -> Address    -- ^ Address of Evaluating node
  -> ByteString -- ^ Current Transaction hash
  -> Address    -- ^ Issuer of transaction (tx origin field)
  -> PrivateKey -- ^ Node private key for signing
  -> Timestamp  -- ^ Contract timestamp
  -> Address    -- ^ Contract owner
  -> World      -- ^ Initial world
  -> Script
  -> IO Contract
scriptToContract blockIdx blockTs nodeAddr txHash txOrigin privKey cTimestamp cOwner world s = do
  (pub,_) <- Homo.genRSAKeyPair Homo.rsaKeySize -- XXX: Actual key of validator
  let evalCtx = EvalCtx
        { currentBlock = blockIdx
        , currentValidator = nodeAddr
        , currentTransaction = txHash
        , currentTimestamp = blockTs
        , currentCreated = cTimestamp
        , currentDeployer = cOwner
        , currentTxIssuer = txOrigin
        , currentAddress = mempty  -- XXX The emptyAddr here is possibly very bad
        , currentPrivKey = privKey
        , currentStorageKey = pub
        }
  gs <- Storage.initStorage evalCtx world s
  let cAddress = Derivation.addrContract' cTimestamp gs
  pure Contract.Contract
    { timestamp        = cTimestamp
    , script           = s
    , localStorage     = Map.empty
    , globalStorage    = gs
    , localStorageVars = Storage.initLocalStorageVars s
    , methods          = Script.methodNames s
    , state            = Graph.GraphInitial
    , owner            = cOwner
    , address          = cAddress
    }

-- | Create a contract and derive an address in the process
createContract
  :: Int64      -- ^ Current Block Index
  -> Timestamp  -- ^ Current Block Timestamp
  -> Address    -- ^ Address of Evaluating node
  -> ByteString -- ^ Current Transaction hash
  -> Address    -- ^ Issuer of transaction (tx origin field)
  -> PrivateKey -- ^ Node private key for signing
  -> Timestamp  -- ^ Contract timestamp
  -> Address    -- ^ Contract owner
  -> World      -- ^ Initial world
  -> Text       -- ^ Raw FCL code
  -> IO (Either Text Contract)
createContract blockIdx blockTs nodeAddr txHash txOrigin privKey cTimestamp cOwner world body = do
  (pub,_) <- Homo.genRSAKeyPair Homo.rsaKeySize -- XXX: Actual key of validator
  let evalCtx = EvalCtx
        { currentBlock = blockIdx
        , currentValidator = nodeAddr
        , currentTransaction = txHash
        , currentTimestamp = blockTs
        , currentCreated = cTimestamp
        , currentDeployer = cOwner
        , currentTxIssuer = txOrigin
        , currentAddress = cAddress
        , currentPrivKey = privKey
        , currentStorageKey = pub
        }
  createContractWithEvalCtx evalCtx world body
  where
    cAddress =
      fromRaw $ b58 $ Hash.sha256Raw $
        mconcat $ map Hash.getHash
          [ Hash.toHash cOwner, Hash.toHash cTimestamp , Hash.toHash body ]

-- XXX old args
--  :: Address         -- ^ Owner Address
--  -> Address         -- ^ Address of Contract
--  -> Time.Timestamp  -- ^ Timestamp of creation
-- | Create a contract with a supplied address
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
