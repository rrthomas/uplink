{-|

Transaction and block validation rules.

This module contains the business logic for validating two data structures with
respect to the chain rules and world state: Blocks and Transactions.

Both Blocks and Transaction have properties necessary to validate, but the crux
of validation is with respect to Transaction Headers. Transaction headers
contain all the data relevant to updating the world state and thus will require
the most validation logic when validating against the current world state.

-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Validate (

  ApplyCtx(..),
  ApplyState(..),

  -- ** Block Validation
  verifyValidateAndApplyBlock,
  validateAndApplyBlock,

  -- ** Transaction Validation
  validateAndApplyTransaction,
  validateTransactionOrigin,
  verifyTransaction,

  -- ** Apply Block to World
  execApplyT,
  initApplyState,
  applyBlock,
  applyTransaction,

) where

import Protolude hiding (get)

import Control.Monad.Base
import Control.Monad.State

import Data.DList (DList)
import qualified Data.DList as DL
import qualified Data.Map as Map

import qualified DB

import Address (Address, AAccount, AContract)
import Asset (createAsset, Holder(..))
import Block (Block(..))
import Contract (Contract)
import Delta (Delta(..))
import Ledger (World)
import Transaction (Transaction(..))
import SafeString (toBytes)

import qualified Key
import qualified Asset
import qualified Block
import qualified Ledger
import qualified Account
import qualified Address
import qualified Storage
import qualified Contract
import qualified Transaction as Tx

import qualified Script
import qualified Script.Compile as C
import Script.Eval (EvalCtx(..))
import qualified Script.Eval as Eval
import qualified Script.Init
import qualified Script.Typecheck as TC

import qualified Consensus.Authority.Params as CAP

-- | Verifies, validates, and applys and validating all transactions
-- Note: Short circuits on un-verifiable transaction, but not on invalid
-- invalid transaction with respect to ledger state (XXX fix?)
verifyValidateAndApplyBlock
  :: DB.MonadReadDB m
  => ApplyState
  -> ApplyCtx
  -> Block
  -> m (Either Block.InvalidBlock (World, Map (Address AContract) [Delta]))
verifyValidateAndApplyBlock applyState applyCtx block =
  case verifyBlock (accumWorld applyState) block of
    Left err -> pure $ Left err
    Right _  -> do
      (newLedgerState, itxs, deltas) <-
        execApplyT applyState applyCtx $
          validateAndApplyBlock block
      case itxs of
        []    -> pure $ Right (newLedgerState, deltas)
        (itx:_) -> pure $ Left $
          Block.InvalidBlock (Block.index block) $
            Block.InvalidBlockTx itx

-------------------------------------------------------------------------------
-- Block Validation w/ Respect to World State
-------------------------------------------------------------------------------

-- | Validate a block w/ Respect to World state and apply transactions in order,
-- accumulating a world state such that each transaction is evaluated in the
-- ledger state context determined by the initial ledger state and all other
-- preceding transactions in the block.
validateAndApplyBlock
  :: DB.MonadReadDB m
  => Block
  -> ApplyT m ()
validateAndApplyBlock block =
  mapM_ validateAndApplyTransaction $
    Block.transactions block

-------------------------------------------------------------------------------
-- Transaction Validation w/ Respect to World State
-------------------------------------------------------------------------------

-- | Validates a transaction in the context of Applying a block of transaction
-- to the ledger state. This function does not short circuit on error, but
-- accumulates them in the ApplyState.
validateAndApplyTransaction
  :: DB.MonadReadDB m
  => Transaction
  -> ApplyT m ()
validateAndApplyTransaction tx = do
  txUnique <- lift $ DB.isTransactionUnique tx
  if txUnique
     then do
       ledgerState <- gets accumWorld
       case validateTransactionOrigin ledgerState tx of
         Left err -> accumInvalidTx err
         Right _  -> applyTransaction tx
     else
       accumInvalidTx $ Tx.InvalidTransaction tx Tx.DuplicateTransaction

-- | Validate a transaction based on world state (Issuer address exists & is valid)
validateTransactionOrigin :: World -> Transaction -> Either Tx.InvalidTransaction ()
validateTransactionOrigin world tx@Transaction{..} =
    case Tx.header tx of
      -- CreateAccount Tx could be self-signed so no need to lookup account
      Tx.TxAccount (Tx.CreateAccount pubKey _ _) -> Right ()
      -- If not self-signed CreateAccount Tx, transaction is invalid
      otherwise -> case Ledger.lookupAccount origin world of
        Left _ -> Left $ mkInvalidTx $ Tx.NoSuchOriginAccount origin
        Right _ -> Right ()
  where
    mkInvalidTx = Tx.InvalidTransaction tx

-------------------------------------------------------------------------------
-- Block & Transaction Verification (Signatures)
-------------------------------------------------------------------------------

-- | Verify block signatures & hash
verifyBlock :: World -> Block -> Either Block.InvalidBlock ()
verifyBlock world block =
  let genAddr = Block.origin (Block.header block) in
  case Ledger.lookupAccount genAddr world of
    Left err  ->
      Left $ mkInvalidBlockErr $
        Block.InvalidBlockOrigin genAddr
    Right acc -> do
      -- Verify all block signatures
      forM_ (Block.signatures block) $ \(Block.BlockSignature signature signerAddr) ->
        case Ledger.lookupAccount signerAddr world of
          Left _    ->
            Left $ mkInvalidBlockErr $
              Block.InvalidBlockSigner signerAddr
          Right acc -> do
            let accPubKey = Account.publicKey acc
            first mkInvalidBlockErr $
              verifyBlockSig accPubKey signature
      -- Verify signatures of all transactions in block
      let blockTxs  = Block.transactions block
      first (mkInvalidBlockErr . Block.InvalidBlockTx) $
        mapM_ (verifyTransaction world) blockTxs
  where
    verifyBlockSig pubKey sig =
      first Block.InvalidBlockSignature $
        Block.verifyBlockSig pubKey sig block

    mkInvalidBlockErr = Block.InvalidBlock (Block.index block)

-- | Verify a transaction signature & hash
verifyTransaction :: World -> Transaction -> Either Tx.InvalidTransaction ()
verifyTransaction world tx@Transaction{..} =
  case header of
    -- In the strange case of a CreateAccount transaction...
    Tx.TxAccount (Tx.CreateAccount pub _ _) ->
      case Key.tryDecodePub (toBytes pub) of
        Left _ -> Left $ mkInvalidTx Tx.InvalidPubKey
        Right pub'
          -- If self-signed, verify sig with pubkey in TxHeader
          | Address.deriveAddress pub' == origin ->
              verifyTxWithPubKey pub'
          -- Otherwise the origin of the transaction must be an existing account
          | otherwise -> verifyTxByAccLookup
    -- Otherwise, Tx origin acc must exist and header must be signed w/ account privkey
    otherwise -> verifyTxByAccLookup
  where
    mkInvalidTx = Tx.InvalidTransaction tx

    verifyTxByAccLookup :: Either Tx.InvalidTransaction ()
    verifyTxByAccLookup =
      case Ledger.lookupAccount origin world of
        Left _    -> Left $ mkInvalidTx $ Tx.NoSuchOriginAccount origin
        Right acc -> verifyTxWithPubKey $ Account.publicKey acc

    verifyTxWithPubKey :: Key.PubKey -> Either Tx.InvalidTransaction ()
    verifyTxWithPubKey pubkey =
      first (mkInvalidTx . Tx.InvalidTxField . Tx.InvalidTxSignature) $
        Tx.verifyTransaction pubkey tx

-------------------------------------------------------------------------------
-- Apply Block Transactions to World State
-------------------------------------------------------------------------------

-- | This datatype hosts values necessary for evaluating the TxAsset Call
-- transaction and applying it to the world state
data ApplyCtx = ApplyCtx
  { applyCurrBlock      :: Block
  , applyNodeAddress    :: (Address AAccount)
  , applyNodePrivKey    :: Key.PrivateKey
  } deriving (Generic)

-- | The state that is being modified during transaction application
data ApplyState = ApplyState
  { accumWorld  :: World                       -- ^ Accumulated ledger state
  , accumErrs   :: DList Tx.InvalidTransaction -- ^ Accumulated list of invalid transactions
  , accumDeltas :: Map (Address AContract) [Delta]         -- ^ Map of Contract Addrs -> Deltas
  }

-- | The Monad used for functions accessing ApplyCtx env and ApplyState state
newtype ApplyT m a = ApplyT { unApplyT :: ReaderT ApplyCtx (StateT ApplyState m) a }
  deriving (Functor, Applicative, Monad, MonadReader ApplyCtx, MonadState ApplyState)

instance MonadBase IO m => MonadBase IO (ApplyT m) where
  liftBase = ApplyT . liftBase

instance MonadTrans ApplyT where
  lift = ApplyT . lift . lift

initApplyState :: World -> ApplyState
initApplyState world = ApplyState
  { accumWorld  = world
  , accumErrs   = mempty
  , accumDeltas = mempty
  }

-- | Evaluate a computation in the ApplyT monad
execApplyT
  :: Monad m
  => ApplyState -- ^ Initial state to modify
  -> ApplyCtx   -- ^ Initial environment values
  -> ApplyT m a -- ^ Computation
  -> m (World, [Tx.InvalidTransaction], Map (Address AContract) [Delta])
execApplyT s r =
    fmap reifyApplyState .
      flip execStateT s .
        flip runReaderT r .
          unApplyT
  where
    -- | Evaluates the DList accumulating errors
    reifyApplyState :: ApplyState -> (World, [Tx.InvalidTransaction], Map (Address AContract) [Delta])
    reifyApplyState (ApplyState w itxs' d) = let itxs = itxs' `DL.apply` [] in (w,itxs,d)

-- | Set the world state
putWorld :: Monad m => World -> ApplyT m ()
putWorld w = modify $ \applyState ->
  applyState { accumWorld = w }

-- | Add an error to ApplyState
accumInvalidTx :: Monad m => Tx.InvalidTransaction -> ApplyT m ()
accumInvalidTx itx = modify $ \applyState ->
  let accumErrs' = accumErrs applyState in
  applyState { accumErrs = accumErrs' `DL.append` DL.fromList [itx] }

-- | Add a list of Deltas to ApplyState
appendDeltas :: Monad m => Address AContract -> [Delta] -> ApplyT m ()
appendDeltas addr ds = modify $ \applyState ->
  let accumDeltas' = accumDeltas applyState in
  applyState {
    accumDeltas =
      Map.insertWith (flip (++)) addr ds accumDeltas'
  }

-------------------------------------------------------------------------------

-- | Applies a block's transactions to the ledger state, accumulating state
-- along the way such that transactions that come later in the block can depend
-- on transaction state modifications performed by txs earlier in the block
applyBlock
  :: MonadBase IO m
  => Block
  -> ApplyT m ()
applyBlock = mapM_ applyTransaction . Block.transactions

-- | Applies the contents of the TxHeader to the world state
applyTransaction
  :: MonadBase IO m
  => Transaction
  -> ApplyT m ()
applyTransaction tx@Transaction{..} =
    case header of

      -- Accounts
      Tx.TxAccount txAccount ->
        applyTxAccount tx txAccount

      -- Transactions
      Tx.TxAsset txAsset ->
        applyTxAsset tx txAsset

      -- Contracts
      Tx.TxContract txContract ->
        applyTxContract tx txContract

-- | Applies TxAsset headers to world state
applyTxAsset
  :: Monad m
  => Transaction
  -> Tx.TxAsset
  -> ApplyT m ()
applyTxAsset tx txAsset = do

  blockTs <- Block.timestamp . Block.header <$> asks applyCurrBlock
  world <- gets accumWorld

  case txAsset of

    Tx.CreateAsset name supply mRef atyp metadata -> do
      let assetAddr = Tx.transactionToAddress tx
          asset = createAsset (toS $ toBytes name) origin supply mRef atyp blockTs assetAddr metadata
       in case Ledger.addAsset assetAddr asset world of
                Left err -> throwInvalidTxAsset $ Tx.AssetError err
                Right newworld -> putWorld newworld

    Tx.Transfer assetAddr toAddr amnt -> do
      let eLedger = Ledger.transferAsset assetAddr (Holder origin) toAddr amnt world
      case eLedger of
        Left err -> throwInvalidTxAsset $ Tx.AssetError err
        Right newWorld -> putWorld newWorld

    Tx.Circulate assetAddr amnt -> do
      let eLedger = Ledger.circulateAsset assetAddr (Tx.origin tx) amnt world
      case eLedger of
        Left err -> throwInvalidTxAsset $ Tx.AssetError err
        Right newWorld -> putWorld newWorld

    Tx.Bind _ _ _ -> pure ()

    Tx.RevokeAsset addr -> do
      let txIssuer = Tx.origin tx
      case Ledger.lookupAsset addr world of
        Left err -> throwInvalidTxAsset $ Tx.AssetError err
        Right asset -> do
          let assetIssuer = Asset.issuer asset
          if assetIssuer /= txIssuer
            then throwInvalidTxAsset $ Tx.AssetError $
              Ledger.RevokerIsNotIssuer txIssuer asset
          else do
            let eLedger = Ledger.removeAsset addr world
            case eLedger of
              Left err -> throwInvalidTxAsset $ Tx.AssetError err
              Right newWorld -> putWorld newWorld

  where
    origin = Tx.origin tx

    throwInvalidTxAsset =
      accumInvalidTx . mkInvalidTx tx . Tx.InvalidTxAsset

-- | Applies TxAccount headers to world state
applyTxAccount
  :: Monad m
  => Transaction
  -> Tx.TxAccount
  -> ApplyT m ()
applyTxAccount tx txAccount = do

  world <- gets accumWorld

  case txAccount of

    Tx.CreateAccount pub tz md ->
      case Key.tryDecodePub (toBytes pub) of
        Left err -> accumInvalidTx $ mkInvalidTx tx $
          Tx.InvalidTxAccount $ Tx.InvalidPubKeyByteString $ toS err
        Right pub' -> do
          let newAccount = Account.createAccount pub' tz md
          let eWorld = first Tx.AccountError $ Ledger.addAccount newAccount world
          case eWorld of
            Left err -> accumInvalidTx $ mkInvalidTx tx $ Tx.InvalidTxAccount err
            Right newWorld -> putWorld newWorld

    Tx.RevokeAccount accAddr ->
      case Ledger.lookupAccount accAddr world of
        Left err -> accumInvalidTx $ mkInvalidTx tx $
          Tx.InvalidTxAccount $ Tx.AccountError err
        Right acc -> do
          currBlock <- asks applyCurrBlock
          let validatorSet' = Block.getValidatorSet currBlock
          let accAddr = Account.address acc
          -- If account is validator account, do not allow revocation
          if accAddr `CAP.isValidatorAddr` validatorSet'
            then accumInvalidTx $ mkInvalidTx tx $
              Tx.InvalidTxAccount $ Tx.RevokeValidatorError accAddr
            else do
              let eWorld = first Tx.AccountError $ Ledger.removeAccount acc world
              case eWorld of
                Left err -> accumInvalidTx $ mkInvalidTx tx $ Tx.InvalidTxAccount err
                Right newWorld -> putWorld newWorld

-- | Applies TxContract headers to world state
applyTxContract
  :: forall m. MonadBase IO m
  => Transaction   -- ^ For error throwing InvalidTransaction
  -> Tx.TxContract
  -> ApplyT m ()
applyTxContract tx txContract = do

  let issuer = Tx.origin tx

  case txContract of

    Tx.CreateContract scriptSS -> do
      let scriptText = decodeUtf8 $ toBytes scriptSS
          contractAddr = Tx.transactionToAddress tx

      world <- gets accumWorld
      applyCtx <- ask
      eContract <- liftBase $
        case C.compile scriptText of
          Left err  -> pure (Left err)
          Right (_, ast) -> do
            let evalCtx = initEvalCtxTxCreateContract applyCtx tx contractAddr ast
            Script.Init.createContractWithEvalCtx evalCtx world ast
      case eContract of
        Left err -> throwTxContract $ Tx.InvalidContract $ toS err
        Right contract -> do
          world <- gets accumWorld
          let eContract = Ledger.addContract contractAddr contract world
          case eContract of
            Left err -> throwTxContract $ Tx.ContractError err
            Right newWorld -> putWorld newWorld

    Tx.Call contractAddr methodNmBS args -> do
      world <- gets accumWorld
      case Ledger.lookupContract contractAddr world of
        Left err -> throwTxContract $ Tx.ContractError err
        Right contract -> do
          let eCallInputs = processCallInputs contract methodNmBS args
          case eCallInputs of
            Left err -> throwTxContract err
            Right method -> applyCall contract method args

    -- XXX Implement SyncLocal
    Tx.SyncLocal _ _ -> return ()
  where
    -- Validate method name being called
    processCallInputs
      :: Contract.Contract
      -> ByteString
      -> [Script.Value]
      -> Either Tx.InvalidTxContract Script.Method
    processCallInputs c methodNmBS args = do
      let methodNm = Script.Name $ decodeUtf8 methodNmBS
          enums = Script.createEnumInfo . Script.scriptEnums . Contract.script $ c
      case Contract.lookupContractMethod methodNm c of
        Left err     -> Left $ Tx.InvalidMethodName err
        Right method -> do
          -- Typecheck method w/ supplied args
          first Tx.InvalidCallArgType $
            TC.tcMethodCall enums method args
          Right method

    applyCall
      :: Contract.Contract -- Contract from which to exec method
      -> Script.Method     -- Method to eval
      -> [Script.Value]    -- Method args
      -> ApplyT m ()
    applyCall contract method argVals = do

      -- Setup evalCtx and evalState
      applyCtx <- ask
      world <- gets accumWorld

      -- Evaluate the method call
      evalRes <- liftBase $ do
        let evalCtx = initEvalCtxTxCall applyCtx contract tx
            evalState = Eval.initEvalState contract world
        Eval.execEvalM evalCtx evalState $
          Eval.evalMethod method argVals

      case evalRes of
        Left err -> throwTxContract $ Tx.EvalFail err
        Right resEvalState -> do

          -- Append deltas to ApplyState
          let deltas = Eval.deltas resEvalState
          appendDeltas (Contract.address contract) deltas

          -- Update world state based on results from evalMethod
          let newWorld = updateWorld contract resEvalState
          putWorld newWorld

    -- Update the contract based on results from applyDeltas
    updateContract :: Eval.EvalState -> Contract -> Contract
    updateContract evalState contract =
      let gstore = Eval.globalStorage evalState
          lstore = Eval.localStorage evalState
      in contract
        { Contract.globalStorage = Storage.GlobalStorage gstore
        , Contract.localStorage  = map Storage.LocalStorage lstore
        , Contract.state         = Eval.graphState evalState
        }

    -- Update the contract in the new world state resulting from Deltas
    updateWorld :: Contract -> Eval.EvalState -> World
    updateWorld c es =
      let newContract = updateContract es c in
      Ledger.updateContract (Contract.address c) newContract $ Eval.worldState es

    -- Error helpers
    throwTxContract :: Tx.InvalidTxContract -> ApplyT m ()
    throwTxContract = accumInvalidTx . mkInvalidTx tx . Tx.InvalidTxContract

    throwTxAsset :: Tx.InvalidTxAsset -> ApplyT m ()
    throwTxAsset = accumInvalidTx . mkInvalidTx tx . Tx.InvalidTxAsset

-- | Intializes an evaluation context from values available
-- to the TxContract Call transaction in `applyTxContract` case
initEvalCtxTxCall
  :: ApplyCtx
  -> Contract
  -> Transaction
  -> Eval.EvalCtx
initEvalCtxTxCall ApplyCtx{..} contract tx =
  EvalCtx
    { currentBlock = fromIntegral $ Block.index applyCurrBlock
    , currentValidator = applyNodeAddress
    , currentTransaction = Tx.hashTransaction tx
    , currentTimestamp = Block.timestamp $ Block.header applyCurrBlock
    , currentCreated = Contract.timestamp contract
    , currentDeployer = Contract.owner contract
    , currentTxIssuer = Tx.origin tx
    , currentAddress = Contract.address contract
    , currentPrivKey = applyNodePrivKey
    , currentHelpers = Script.scriptHelpers (Contract.script contract)
    }

initEvalCtxTxCreateContract
  :: ApplyCtx
  -> Transaction
  -> Address AContract
  -> Script.Script
  -> Eval.EvalCtx
initEvalCtxTxCreateContract ApplyCtx{..} tx contractAddr script =
  EvalCtx
    { currentBlock = fromIntegral $ Block.index applyCurrBlock
    , currentValidator = applyNodeAddress
    , currentTransaction = Tx.hashTransaction tx
    , currentTimestamp = Block.timestamp $ Block.header applyCurrBlock
    , currentCreated = Block.timestamp $ Block.header applyCurrBlock
    , currentDeployer = Tx.origin tx
    , currentTxIssuer = Tx.origin tx
    , currentAddress = contractAddr
    , currentPrivKey = applyNodePrivKey
    , currentHelpers = Script.scriptHelpers script
    }

-- | Helper to construct InvalidTransactions from InvalidTxHeaders
mkInvalidTx :: Transaction -> Tx.InvalidTxHeader -> Tx.InvalidTransaction
mkInvalidTx tx = Tx.InvalidTransaction tx . Tx.InvalidTxHeader
