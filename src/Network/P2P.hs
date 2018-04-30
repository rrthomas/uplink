{-|

Peer to peer network interface.

-}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Network.P2P (
  -- ** Server
  p2p,
  createLocalNode,
  createLocalNode'
) where

import Protolude hiding (newChan, catch)

import Control.Distributed.Process.Lifted
import Control.Distributed.Process.Lifted.Class
import qualified Control.Distributed.Process.Node as DPN

import Network.Socket (HostName, ServiceName)
import qualified Network.Transport.TCP as TCP
import qualified Network.Transport.InMemory as Mem

import qualified Data.Binary as Binary
import qualified Data.Map as Map

import DB
import NodeState
import qualified Key
import qualified Utils
import qualified SafeString
import qualified Config
import qualified Account
import qualified Block
import qualified Transaction

import qualified Network.P2P.Cmd as Cmd
import qualified Network.P2P.Consensus as P2PConsensus
import qualified Network.P2P.Simulate as Simulate
import Network.P2P.Controller
import Network.P2P.Send (MatchProcBase(..), receiveWaitProcBase)
import Network.P2P.SignedMsg (nsendPeersSigned)
import qualified Network.P2P.Logging as Log
import qualified Network.P2P.Message as Message
import Network.P2P.Service (Service(..))
import Network.Utils (waitForLocalService, waitForLocalService')

-------------------------------------------------------------------------------
-- Node Creation
-------------------------------------------------------------------------------

data Mode
  = InMemory
  | TCP
  deriving (Eq, Show)

-- | Creates a local TCP cloud haskell node using from Config
createLocalNode :: Config.Config -> IO DPN.LocalNode
createLocalNode config =
    createLocalNode' hostname port (Just DPN.initRemoteTable)
  where
    hostname = toS $ Config.hostname config
    port     = show $ Config.port config

-- | Creates a local TCP cloud haskell node
createLocalNode'
  :: HostName
  -> ServiceName
  -> Maybe RemoteTable
  -> IO DPN.LocalNode
createLocalNode' host port mRemoteTable = do
  let transport = TCP
  let rTable = fromMaybe DPN.initRemoteTable mRemoteTable
  case transport of

    InMemory ->  do
      transport <- liftIO $ Mem.createTransport
      DPN.newLocalNode transport rTable

    TCP -> do
      mtransport <- TCP.createTransport host port (const (host,port)) TCP.defaultTCPParameters
      case mtransport of
        Left err        -> do
          liftIO $ Utils.dieRed "Could not bind socket on port. Now I die..."
        Right transport -> DPN.newLocalNode transport rTable

-------------------------------------------------------------------------------
-- Node
-------------------------------------------------------------------------------

{-

 Processes
 =========

  p2p                    chan / console
    |                         |
    +---> p2p:controller      |
    +---> test/messaging      |
    +---> tasks  <------------+
    +---> consensus

-}

-- | P2P Service initialization
p2p
  :: (MonadReadWriteDB m, MonadProcessBase m)
  => Config.Config
  -> NodeT m ()
p2p config = do
  -- Initialize logging process (before any other process)
  unregister (show Logger)
  let loggingRules = Config.loggingRules config
  spawnLocal (Log.loggerProc loggingRules) >>= register (show Logger)
  -- Initialize PeerController process
  spawnLocal (peerControllerProc $ Config.bootnodes config)
  -- Run main process
  waitForLocalService' PeerController 1000000 mainProcess

mainProcess
  :: forall m. (MonadReadWriteDB m, MonadProcessBase m)
  => NodeT m ()
mainProcess = do
  register "main" =<< getSelfPid

  isTestNode <- NodeState.isTestNode
  let msgService = if isTestNode then TestMessaging else Messaging
  isValidatingNode <- NodeState.isValidatingNode

  if isTestNode
    then Log.info "Node is running in test mode."
    else Log.info "Node is running in production mode."

  nodeAddr <- NodeState.askSelfAddress
  Log.info $ "Node account address: " <> show nodeAddr

  -- Spawn process to handle network consensus if validating node
  mConsProc <-
    if isValidatingNode
      then do
        let consProc = P2PConsensus.consensusProc msgService
        pure $ Just (Consensus, consProc)
      else -- Non validating nodes do not need consensus process
        return Nothing

  -- Construct Process to be supervised
  let procList = maybeToList mConsProc ++                -- Consensus Proc
        [ (Tasks, Cmd.tasksProc msgService)              -- Cmd Proc
        , (msgService, Message.messagingProc msgService) -- Messaging Proc
        , (Simulation, Simulate.simulationProc)          -- Contract Simulation Proc
        ]

  -- Spawn Supervisor Process
  supervisorPid <- spawnLocal $ supervisorProc procList

  -- Join Network after waiting for Messaging proc to spawn:
  Log.info "Joining the Uplink network..."
  mres <- waitForLocalService msgService 3000000 $ do
    -- Submit CreateAccount Tx to network if new account
    if isValidatingNode
      then Log.info "I am a validating node, according to the genesis block."
      else submitNewNodeAccountTx msgService
    -- Join the network by asking for next block
    lastBlockIdx <- Block.index <$> NodeState.getLastBlock
    getBlockAtIdx1Msg <- Message.mkGetBlockAtIdxMsg $ lastBlockIdx + 1
    nsendPeersSigned msgService getBlockAtIdx1Msg

  case mres of
    Nothing -> do
      Log.critical "Failed to join network. Terminating."
      terminate
    Just _  ->
      -- Don't terminate main process
      forever $ liftIO $
        threadDelay 1000000

  where
    submitNewNodeAccountTx :: Service -> NodeT m ()
    submitNewNodeAccountTx msgService = do
      -- If node is new to network,
      -- submit CreateAccount transaction
      accountType <- NodeState.askAccountType
      case accountType of
        NodeState.Existing -> return ()
        NodeState.New      -> do
          nodeAcc <- NodeState.askAccount
          let pub      = SafeString.fromBytes' $ Key.unHexPub $ Key.encodeHexPub $ Account.publicKey nodeAcc
              tz       = Account.timezone nodeAcc
              md       = Account.metadata nodeAcc
              accTxHdr = Transaction.TxAccount $ Transaction.CreateAccount pub tz md
          newAccTx <- Cmd.newTransaction accTxHdr
          Cmd.nsendTransaction msgService newAccTx

-------------------------------------------------------------------------------
-- Supervisor Processes
--
-- This process "manages" all Uplink processes, spawning them initially and
-- monitoring them throughout the lifetime of the uplink node. If a process
-- crashes, it reports the reason for the crash, unregisters the process name,
-- and cleans up accordingly, finally spawning another copy of the process in
-- it's place.
--
-- This was commissioned after some processes were silently dying when fatal
-- exceptions werent being caught. Now, if a process throws an unexpected,
-- uncaught exception, the crash will be discovered, reported, and the process
-- will be brough back to life by the supervisor process.
-------------------------------------------------------------------------------

data ServiceInfo m where
    ServiceInfo :: MonadProcessBase m =>
      { service    :: Service
      , monitorRef :: MonitorRef
      , process    :: NodeT m ()
      } -> ServiceInfo m

type ProcessMap m = Map ProcessId (ServiceInfo m)

data KillAllProcs = KillAllProcs
  deriving (Generic, Binary.Binary)

-- | This process monitors all other uplink processes. It does not used the
-- `SignedMsg` interface, because all messages this process handles are sent
-- from the local node itself and does not communicate with any remote
-- processes.
supervisorProc
  :: forall m. MonadProcessBase m
  => [(Service, NodeT m ())]
  -> NodeT m ()
supervisorProc procs = do
    register "supervisor" =<< getSelfPid
    -- Spawn all processes
    procInfos <- mapM (uncurry spawnService) procs
    -- Create MVar for managing live processes
    procsMVar <- liftIO $ newMVar $ Map.fromList procInfos

    -- Wait for ProcessMonitorNotifications or QUIT from cmd line
    forever $ receiveWaitProcBase
      [ MatchProcBase $ onMonitorNotif procsMVar
      , MatchProcBase $ onKillAllProcs procsMVar
      ]
  where
    onMonitorNotif
      :: MVar (ProcessMap m)
      -> ProcessMonitorNotification
      -> NodeT m ()
    onMonitorNotif procsMVar (ProcessMonitorNotification mref oldPid died) = do
      procMap <- liftIO $ readMVar procsMVar
      case Map.lookup oldPid procMap of
        Nothing -> do
          Log.warning $
            "[Supervisor] Unrecognized processId on monitor: " <> show oldPid
          unmonitor mref -- unmonitor the unrecognized process
        Just servInfo@(ServiceInfo s mref proc) -> do
          let servName = show s
          -- Fully kill the process
          Log.warning $ "[Supervisor] " <> servName <> " service died:\n   " <> show died
          killService oldPid servInfo
          -- Start new instance of same process
          (newPid, newServInfo) <- spawnService s proc
          Log.warning $ "[Supervisor] Spawned new service: " <> servName
          -- Update the managed processes MVar to reflect new process
          liftIO $ modifyMVar_ procsMVar $ \procMap' ->
            pure $ Map.insert newPid newServInfo $ Map.delete oldPid procMap'

    onKillAllProcs
      :: MVar (ProcessMap m)
      -> KillAllProcs
      -> NodeT m ()
    onKillAllProcs procsMVar _ = do
      procs <- liftIO $ readMVar procsMVar
      mapM_ (uncurry killService) $ Map.toList procs

-- | Spawn a process, registering it with the given name and
-- monitoring it. The child process spawned will report
-- `ProcessMonitorNotification`s to the process that calls this function.
spawnService
  :: (MonadProcessBase m)
  => Service
  -> NodeT m ()
  -> NodeT m (ProcessId, ServiceInfo m)
spawnService service nproc = do
    Log.info $ "Spawning service: " <> show service
    pid <- spawnLocal nproc
    register (show service) pid
    mref <- do
      waitSpawn pid
      monitor pid
    pure (pid, ServiceInfo service mref nproc)
  where
    waitSpawn pid = do
      isAlive <- whereis (show service)
      case isAlive of
        Nothing -> do
          liftIO $ threadDelay 10000
          waitSpawn pid
        Just _  -> Log.info $
          show service <> " spawned successfully!"

-- | Cleanup from a spawnService call
killService
  :: MonadProcessBase m
  => ProcessId
  -> ServiceInfo m
  -> NodeT m ()
killService pid (ServiceInfo service mref _) = do
  let serviceName = show service
  let killMsg = "Killing service: " <> serviceName

  -- Make sure it's dead, and unregister the process
  isAlive <- whereis (show service)
  case isAlive of
    Nothing  -> pure ()
    Just _   -> unregister (show service)

  Log.warning killMsg
  kill pid $ toS killMsg
  unmonitor mref
