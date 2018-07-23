{-

Process to simulate FCL contract execution

-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}

module Network.P2P.Simulate (
  Simulation(..),

  SimulationMsg(..),
  SimulationError(..),
  SimulationSuccess(..),
  SimKey,
  CreateSimulation(..),
  CreateSimulationError(..),
  CreateSimulationSuccess(..),

  UpdateSimulation(..),
  UpdateSimulationError(..),
  ModifyTimestamp'(..),
  CallMethod'(..),

  QuerySimulation(..),
  QuerySimulationError(..),
  QuerySimulationSuccess(..),
  QueryMethods'(..),
  QueryContract'(..),
  QueryLedgerState'(..),
  QueryAssets'(..),

  CallMethodError(..),

  commSimulationProc,
  commSimulationProc',

  commSimulationRemoteProc
) where

import Protolude hiding (Type, TypeError, put, get, evalState, newChan)

import Control.Arrow ((&&&))
import Control.Monad.Base (MonadBase, liftBase)

import Control.Distributed.Process.Lifted hiding ((.:))
import Control.Distributed.Process.Lifted.Class

import Data.Aeson ((.:), (.:?), parseJSON, object, (.=))
import Data.Aeson.Types (typeMismatch)
import qualified Data.Aeson as A
import qualified Data.Binary as B
import qualified Data.Map
import qualified Data.Serialize as S
import qualified Data.Map as M

import qualified Datetime.Types as D

import Address     (Address, AAsset, AAccount, AContract)
import Asset       (Asset)
import Ledger      (World(..), lookupAsset)
import Block       (Block(..), BlockHeader(..))
import SafeString  (SafeString, toBytes)
import Script      (Value, Name, Method(..), Type, TimeDelta(..), argtys, createEnumInfo, scriptEnums, scriptHelpers)
import Script.Typecheck (TypeErrInfo, tcMethodCall)
import Script.Init (createFauxContract)
import Script.Eval (EvalCtx, EvalState, runEvalM, eval, initEvalState)
import Script.Error (EvalFail)
import Script.Pretty (prettyPrint)

import Script.Parser (parseTimeDelta)
import NodeState   (NodeT, getLedger)
import qualified Utils

import qualified Network.Utils as NUtils

import qualified Contract as C
import qualified NodeState as N
import qualified Storage as Store
import qualified Time
import Script.Eval (EvalCtx(..))
import qualified Script.Eval as Eval
import qualified Hash

import Network.P2P.Service (Service(..), ServiceSpec(Worker))

{-

The contract simulation process receives `SimulationMsg`s and responds with
`SimulationResult`s`. There are three main message types that this process receives:

  > `CreateSimulationMsg`
  > `UpdateSimulationMsg`
  > `QuerySimulationMsg`

Nested in each of these types are the specific actions that can be performed by the
simulation process. Each message type should have a Success and Error type associated
with it, encapsulating the ways in which the simulation can succeed or fail.

The simulation process is synchronous, and expects `SimulationMsg`s to arrive paired
with a `SendPort SimulationResult`: `(SendPort SimulationResponse, SimulationMsg)`.


-}

--------------------------------------------------------------------------------
-- Type class/family that associates Messages with actions to simulate
--------------------------------------------------------------------------------

class (B.Binary a, Typeable a) => Simulatable a where
  type SimulationResult a
  simulate :: SimulatedContract -> a -> IO (SimulationResult a)

--------------------------------------------------------------------------------
-- Top level message wrapper
--------------------------------------------------------------------------------

type SimKey = Address AContract

data SimulationMsg
  = CreateSimulationMsg        CreateSimulation
  | UpdateSimulationMsg SimKey UpdateSimulation
  | QuerySimulationMsg  SimKey QuerySimulation
  deriving (Show, Generic, Typeable, B.Binary)

data SimulationSuccess
  = CreateSimulationSuccess CreateSimulationSuccess -- ^ Result of creating a simulation
  | UpdateSimulationSuccess                         -- ^ Simulation updates have no result
  | QuerySimulationSuccess  QuerySimulationSuccess  -- ^ Result of query of contract simulation state
  deriving (Show, Generic, Typeable, B.Binary)

data SimulationError
  = SimulationDoesNotExist SimKey
  | CreateSimulationError  CreateSimulationError
  | UpdateSimulationError  UpdateSimulationError
  | QuerySimulationError   QuerySimulationError
  deriving (Show, Generic, Typeable, B.Binary)

type SimulationMsgResult = Either SimulationError SimulationSuccess

--------------------------------------------------------------------------------
-- Message to create a new contract simulation
-- Note: This action is not `Simulatable` because it is creating the simulation
--------------------------------------------------------------------------------

data CreateSimulation = CreateSimulation (Address AAccount) SafeString (Maybe World)
  deriving (Show, Generic, Typeable, S.Serialize)

newtype CreateSimulationSuccess = SimulationCreated SimKey
  deriving (Show, Generic, Typeable, B.Binary)

data CreateSimulationError
  = CompilationFail [Char]
  deriving (Show, Generic, Typeable, S.Serialize)

type CreateSimulationResult = Either CreateSimulationError CreateSimulationSuccess

--------------------------------------------------------------------------------

instance B.Binary CreateSimulation where
  put = Utils.putBinaryViaSerialize
  get = Utils.getBinaryViaSerialize

instance B.Binary CreateSimulationError where
  put = Utils.putBinaryViaSerialize
  get = Utils.getBinaryViaSerialize

--------------------------------------------------------------------------------
-- Messages to update the state of a simulated contract
--------------------------------------------------------------------------------

data UpdateSimulation
  = ModifyTimestamp ModifyTimestamp'
  | CallMethod   CallMethod'
  deriving (Show, Generic, Typeable, B.Binary)

instance Simulatable UpdateSimulation where
  type SimulationResult UpdateSimulation = UpdateSimulationResult
  simulate sc = \case
    ModifyTimestamp mts -> Right <$> simulate sc mts
    CallMethod      cmm -> first CallMethodError <$> simulate sc cmm

data UpdateSimulationError
  = CallMethodError CallMethodError
  deriving (Show, Generic, Typeable, B.Binary)

type UpdateSimulationResult = Either UpdateSimulationError SimulatedContract

-- Modify the simulation's timestamp
--------------------------------------------------------------------------------

data ModifyTimestamp'
  = SetTimestamp D.Datetime -- ^ Set the timestamp to a specific datetime
  | AddTimeDelta D.Delta    -- ^ Add a time delta to the eval timestamp
  deriving (Show, Generic, Typeable, B.Binary)

instance Simulatable ModifyTimestamp' where
  type SimulationResult ModifyTimestamp' = SimulatedContract
  simulate sc = \case
    SetTimestamp datetime -> pure $
      let ts = Time.datetimeToPosixMicroSecs datetime
          evalCtx' = (evalCtx sc) { Eval.currentTimestamp = ts }
       in sc { evalCtx = evalCtx' }
    AddTimeDelta delta -> pure $
      let currDt = Time.posixMicroSecsToDatetime $ Eval.currentTimestamp $ evalCtx sc
          newTs  = Time.datetimeToPosixMicroSecs $ D.add currDt delta
          evalCtx' = (evalCtx sc) { Eval.currentTimestamp = newTs }
       in sc { evalCtx = evalCtx' }

-- Call a contract method
--------------------------------------------------------------------------------

data CallMethod' = CallMethod'
  { caller     :: Address AAccount
  , methodName :: Name
  , methodArgs :: [Value]
  } deriving (Show, Generic, Typeable, S.Serialize)

instance B.Binary CallMethod' where
  put = Utils.putBinaryViaSerialize
  get = Utils.getBinaryViaSerialize

instance Simulatable CallMethod' where
  type SimulationResult CallMethod' = Either CallMethodError SimulatedContract
  simulate sc (CallMethod' caller nm args) = do
      -- Lookup method in contract by name
      case C.lookupContractMethod nm c of
        Left err -> pure $ Left $ InvalidMethodName err
        Right method -> do
          -- Typecheck args before evaluation
          let tcRes = tcMethodCall cEnumInfo method args
          case tcRes of
            Left err -> pure $ Left $ InvalidArgType err
            Right _ -> do
              eRes <- -- Evaluate method call
                fmap (fmap snd) $
                  runEvalM evalCtx' evalState $
                    eval c nm args
              pure $ case eRes of
                Left err         -> Left $ EvalFail err
                Right evalState' -> do
                  -- Update contract state with result from method call
                  let c' = updateContract evalState' c
                  Right $ sc { evalState = evalState', contract = c' }
    where
      (SimulatedContract c evalState evalCtx) = sc
      evalCtx' = evalCtx { Eval.currentTxIssuer = caller }

      cEnumInfo = createEnumInfo $ scriptEnums $ C.script c

      -- Update the contract based on result of method call
      updateContract :: Eval.EvalState -> C.Contract -> C.Contract
      updateContract evalState contract =
        let gstore = Eval.globalStorage evalState
            lstore = Eval.localStorage evalState
        in contract
          { C.globalStorage = Store.GlobalStorage gstore
          , C.localStorage  = map Store.LocalStorage lstore
          , C.state         = Eval.graphState evalState
          }

data CallMethodError
  = InvalidArgType TypeErrInfo
  | InvalidMethodName C.InvalidMethodName
  | EvalFail EvalFail
  deriving (Show, Generic, Typeable, S.Serialize)

instance B.Binary CallMethodError where
  put = Utils.putBinaryViaSerialize
  get = Utils.getBinaryViaSerialize

--------------------------------------------------------------------------------
-- Methods to Query information about a simulated contract's state
--------------------------------------------------------------------------------

data QuerySimulation
  = QueryContract      QueryContract'
  | QueryMethods       QueryMethods'
  | QueryAssets        QueryAssets'
  | QueryAsset         QueryAsset'
  | QueryLedgerState   QueryLedgerState'
  deriving (Show, Generic, Typeable, B.Binary)

data QuerySimulationError
  = QueryAssetError QueryAssetError
  deriving (Show, Generic, Typeable, B.Binary)

type QuerySimulationResult = Either QuerySimulationError QuerySimulationSuccess

instance Simulatable QuerySimulation where
  type SimulationResult QuerySimulation = QuerySimulationResult
  simulate sc = \case
    QueryContract    qc   -> Right . Contract         <$> simulate sc qc
    QueryMethods     qmm  -> Right . ContractMethods  <$> simulate sc qmm
    QueryAssets      qam  -> Right . SimulationAssets <$> simulate sc qam
    QueryAsset       qam  -> bimap QueryAssetError SimulationAsset <$> simulate sc qam
    QueryLedgerState qlsm -> Right . LedgerState      <$> simulate sc qlsm

data QuerySimulationSuccess
  = Contract C.Contract
  | ContractMethods  [(Name,[(Name,Type)])]
  | SimulationAssets [Asset]
  | SimulationAsset  Asset
  | LedgerState      World
  deriving (Show, Generic, Typeable, S.Serialize)

instance B.Binary QuerySimulationSuccess where
  put = Utils.putBinaryViaSerialize
  get = Utils.getBinaryViaSerialize

-- Query a contract
--------------------------------------------------------------------------------

data QueryContract' = QueryContract'
  deriving (Show, Generic, Typeable, B.Binary)

instance Simulatable QueryContract' where
  type SimulationResult QueryContract' = C.Contract
  simulate sc QueryContract' = pure $ contract sc

-- Query a contract's methods
--------------------------------------------------------------------------------

data QueryMethods' = QueryMethods'
  deriving (Show, Generic, Typeable, B.Binary)

instance Simulatable QueryMethods' where
  type SimulationResult QueryMethods' = [(Name,[(Name,Type)])]
  simulate sc QueryMethods' =
    pure $ map (Script.methodName &&& argtys) $
      C.callableMethods (contract sc)

-- Query the simulation's assets
--------------------------------------------------------------------------------

data QueryAssets' = QueryAssets'
  deriving (Show, Generic, Typeable, B.Binary)

instance Simulatable QueryAssets' where
  type SimulationResult QueryAssets' = [Asset]
  simulate sc QueryAssets' = pure $
    M.elems . Ledger.assets . Eval.worldState $ evalState sc

-- Query a specific simulation asset by address
--------------------------------------------------------------------------------

data QueryAsset' = QueryAsset' (Address AAsset)
  deriving (Show, Generic, Typeable, B.Binary)

data QueryAssetError = AssetDoesNotExist (Address AAsset)
  deriving (Show, Generic, Typeable, B.Binary)

instance Simulatable QueryAsset' where
  type SimulationResult QueryAsset' = Either QueryAssetError Asset
  simulate sc (QueryAsset' addr) = do
    let world = Eval.worldState $ evalState sc
    pure $ case Ledger.lookupAsset addr world of
      Left err    -> Left (AssetDoesNotExist addr)
      Right asset -> Right asset

-- Query the entire Ledger State
--------------------------------------------------------------------------------

data QueryLedgerState' = QueryLedgerState'
  deriving (Show, Generic, Typeable, B.Binary)

instance Simulatable QueryLedgerState' where
  type SimulationResult QueryLedgerState' = World
  simulate sc QueryLedgerState' = pure $ Eval.worldState $ evalState sc

--------------------------------------------------------------------------------
-- Simulation Service Definition
--------------------------------------------------------------------------------

data Simulation = Simulation
  deriving (Show, Generic, B.Binary)

instance Service Simulation where
  serviceSpec _ = Worker simulationProc

--------------------------------------------------------------------------------
-- Simulation Process
--------------------------------------------------------------------------------

-- | Representing a contract and it's environment
data SimulatedContract
  = SimulatedContract
    { contract  :: C.Contract
    , evalState :: EvalState
    , evalCtx   :: EvalCtx
    }

-- | A map of contract addresses to simulated contracts
type Simulations = Map SimKey SimulatedContract

-- | Process receving SimulationMsgs
simulationProc :: MonadProcessBase m => NodeT m ()
simulationProc = do
  simulations <- liftIO $ newMVar mempty
  controlP $ \runInBase -> forever $ do
    runInBase $ expect >>=
      handleSimulationMsg simulations

handleSimulationMsg
  :: forall m. (MonadProcessBase m)
  => MVar Simulations
  -> (SendPort SimulationMsgResult, SimulationMsg)
  -> NodeT m ()
handleSimulationMsg sims (sp, simMsg) =

  case simMsg of

    CreateSimulationMsg csm -> do
      eRes <-
        bimap CreateSimulationError CreateSimulationSuccess <$>
          createSimulation csm sims
      sendChan sp eRes

    UpdateSimulationMsg sk updateSim -> do
      eRes <- liftIO $
        fmap (second (const UpdateSimulationSuccess)) $
          modifySimulatedContract sk sims $ \sc ->
            first UpdateSimulationError <$>
              simulate sc updateSim
      sendChan sp eRes

    QuerySimulationMsg sk querySim -> do
      eRes <- liftIO $
        withSimulatedContract sk sims $ \sc ->
          bimap QuerySimulationError QuerySimulationSuccess <$>
            simulate sc querySim
      sendChan sp eRes
  where
    createSimulation
      :: forall m. MonadBase IO m
      => CreateSimulation
      -> MVar Simulations
      -> NodeT m (Either CreateSimulationError CreateSimulationSuccess)
    createSimulation (CreateSimulation issuer scriptSS mWorld) sims = do
        world <-
          case mWorld of
            Nothing    -> getLedger
            Just world -> pure world
        lastBlock <- N.getLastBlock
        let blockIdx = fromIntegral $ Block.index lastBlock
            blockTs  = Block.timestamp $ Block.header lastBlock
        nodeAddr <- N.askSelfAddress
        nodePrivKey <- N.askPrivateKey
        nodeAddr <- N.askSelfAddress
        contractTs <- liftBase Time.now
        let rawScript = toS $ SafeString.toBytes scriptSS

        eContract
          <- liftBase
             $ createFauxContract
                 blockIdx
                 blockTs
                 nodeAddr
                 (Hash.toHash ("SIMULATION-TRANSACTION" :: ByteString))
                 issuer
                 nodePrivKey
                 contractTs
                 issuer
                 world
                 rawScript
        case eContract of
          Left err -> pure $ Left $ CompilationFail $ toS err
          Right contract -> do
            evalCtx'   <- initEvalCtx contract
            let evalState'  = initEvalState contract world
                simContract = SimulatedContract contract evalState' evalCtx'
                simKey      = C.address contract
            liftBase $ insertSimulatedContract simKey simContract sims
            pure $ Right $ SimulationCreated simKey
      where
        -- Setup the initial EvalCtx using the current node state
        initEvalCtx :: C.Contract -> NodeT m EvalCtx
        initEvalCtx contract = do
          lastBlock <- N.getLastBlock
          let blockIdx = fromIntegral $ Block.index lastBlock
              blockTs  = Block.timestamp $ Block.header lastBlock
          nodeAddr <- N.askSelfAddress
          nodePrivKey <- N.askPrivateKey
          pure EvalCtx
            { currentBlock = blockIdx
            , currentValidator = nodeAddr
            , currentTransaction = Hash.toHash ("SIMULATION-TRANSACTION" :: ByteString)
            , currentTimestamp = blockTs
            , currentCreated = C.timestamp contract
            , currentDeployer = C.owner contract
            , currentTxIssuer = issuer
            , currentAddress = C.address contract
            , currentPrivKey = nodePrivKey
            , currentHelpers = scriptHelpers (C.script contract)
            }

--------------------------------------------------------------------------------

-- | Send a SimulateMsg to the "simulation" process and wait for a SimulationResult
-- Note: This function is blocking and will wait forever for a response. Use
-- `commSimulationProc'` to specify a timeout for how long to wait for a response.
commSimulationProc
  :: MonadProcessBase m
  => SimulationMsg
  -> m SimulationMsgResult
commSimulationProc simMsg = do
  (sp,rp) <- newChan
  nsend (show Simulation) (sp, simMsg)
  receiveChan rp

-- | Send a SimulateMsg to the "simulation" process and wait for a SimulationResult
-- Note: This function is blocking and will wait `timeout` ms for a response.
commSimulationProc'
  :: MonadProcessBase m
  => Int
  -> SimulationMsg
  -> m (Maybe SimulationMsgResult)
commSimulationProc' timeout simMsg = do
  (sp,rp) <- newChan
  nsend (show Simulation) (sp, simMsg)
  receiveChanTimeout timeout rp

-- | Like `commSimulationProc` but sends to a remote simulation process.
commSimulationRemoteProc
  :: MonadProcessBase m
  => ProcessId
  -> SimulationMsg
  -> m SimulationMsgResult
commSimulationRemoteProc = NUtils.commProc


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

lookupSimulatedContract :: SimKey -> MVar Simulations -> IO (Maybe SimulatedContract)
lookupSimulatedContract key = fmap (Data.Map.lookup key) . readMVar

insertSimulatedContract :: SimKey -> SimulatedContract -> MVar Simulations -> IO ()
insertSimulatedContract  key simContract sims =
  modifyMVar_ sims (pure . M.insert key simContract)

withSimulatedContract
  :: SimKey
  -> MVar Simulations
  -> (SimulatedContract -> IO (Either SimulationError a))
  -> IO (Either SimulationError a)
withSimulatedContract key sims f = do
  mSimContract <- lookupSimulatedContract key sims
  maybe (pure $ Left $ SimulationDoesNotExist key) f mSimContract

modifySimulatedContract
  :: SimKey
  -> MVar Simulations
  -> (SimulatedContract -> IO (Either SimulationError SimulatedContract))
  -> IO (Either SimulationError ())
modifySimulatedContract key sims f =
    withSimulatedContract key sims $ \sc ->
      f sc >>= \case
        Left err  -> pure $ Left err
        Right sc' -> Right <$> modifyMVar_ sims (pure . M.insert key sc')

--------------------------------------------------------------------------------
-- To/FromJSON instances
--------------------------------------------------------------------------------

instance A.FromJSON SimulationMsg where
  parseJSON = \case
    A.Object v -> do
      tag      <- v .: "tag"
      case tag of
        "CreateSimulationMsg" ->
          CreateSimulationMsg <$> v .: "contents"
        "UpdateSimulationMsg" -> do
          contents <- v .: "contents"
          UpdateSimulationMsg
            <$> contents .: "simKey"
            <*> contents .: "contents"
        "QuerySimulationMsg"  -> do
          contents <- v .: "contents"
          QuerySimulationMsg
            <$> contents .: "simKey"
            <*> contents .: "contents"
        invalid -> typeMismatch "SimulateMsg" invalid
    invalid -> typeMismatch "SimulateMsg" invalid

instance A.FromJSON CreateSimulation where
  parseJSON = \case
    A.Object v ->
      CreateSimulation
        <$> v .: "issuer"
        <*> v .: "fcl"
        <*> v .:? "world"
    invalid  -> typeMismatch "CreateSimulation" invalid

instance A.FromJSON UpdateSimulation where
  parseJSON = \case
    A.Object v -> do
      tag      <- v .: "tag"
      case tag of
        "ModifyTimestamp" -> do
          contents <- v .: "contents"
          pure $ ModifyTimestamp contents
        "CallMethod"   -> do
          c <- v .: "contents"
          fmap CallMethod $
            CallMethod'
              <$> c .: "caller"
              <*> c .: "methodName"
              <*> c .: "methodArgs"
        invalid -> typeMismatch "UpdateSimulation" invalid
    invalid -> typeMismatch "UpdateSimulation" invalid

instance A.FromJSON ModifyTimestamp' where
  parseJSON = \case
    A.Object v -> do
      tag      <- v .: "tag"
      case tag of
        "SetTimestamp" -> do
          datetime <- v .: "contents"
          pure $ SetTimestamp datetime
        "AddTimeDelta" -> do
          eTimeDelta <- parseTimeDelta <$> v .: "contents"
          case eTimeDelta of
            Left err -> do
              let errMsg = "AddTimeDelta: " <> show err
              typeMismatch errMsg (A.Object v)
            Right (TimeDelta delta) -> pure $ AddTimeDelta delta
        invalid -> typeMismatch "UpdateSimulation" invalid
    invalid -> typeMismatch "ModifyTimestamp" invalid

instance A.FromJSON QuerySimulation where
  parseJSON = \case
    A.Object v -> do
      tag <- v .: "tag"
      case tag of
        "QueryContract" ->
          pure $ QueryContract QueryContract'
        "QueryMethods" ->
          pure $ QueryMethods QueryMethods'
        "QueryAssets" ->
          pure $ QueryAssets QueryAssets'
        "QueryAsset" -> do
          contents <- v .: "contents"
          pure $ QueryAsset $ QueryAsset' contents
        "QueryLedgerState" ->
          pure $ QueryLedgerState QueryLedgerState'
        invalid -> typeMismatch "QuerySimulation" invalid
    invalid  -> typeMismatch "QuerySimulation" invalid

--------------------------------------------------------------------------------

instance A.ToJSON SimulationSuccess where
  toJSON = \case
    CreateSimulationSuccess css -> A.toJSON css
    UpdateSimulationSuccess     -> "UpdateSimulationSuccess"
    QuerySimulationSuccess qss  -> A.toJSON qss

instance A.ToJSON CreateSimulationSuccess where
  toJSON (SimulationCreated hash) = object ["simKey" .= hash]

instance A.ToJSON QuerySimulationSuccess where
  toJSON (Contract c)     = A.toJSON c
  toJSON (ContractMethods methodSigs) = A.toJSON $
    flip map methodSigs $ \(nm,atys) ->
      (prettyPrint nm, map (bimap prettyPrint prettyPrint) atys)
  toJSON (SimulationAssets as) = A.toJSON as
  toJSON (SimulationAsset a)   = A.toJSON a
  toJSON (LedgerState world)   = A.toJSON world
