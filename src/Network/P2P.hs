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
  createLocalNode
) where

import Protolude hiding (newChan, catch)

import Control.Arrow ((&&&))
import Control.Monad.Base
import Control.Monad.Catch (catch)

import Control.Distributed.Process.Lifted
import Control.Distributed.Process.Lifted.Class
import qualified Control.Distributed.Process.Node as DPN
import qualified Control.Distributed.Process.Node.Lifted as DPNL
import Control.Distributed.Process.Serializable (Serializable)

import qualified Control.Concurrent.Chan as Chan

import Network (PortID(..), PortNumber, connectTo)
import Network.Socket (HostName, ServiceName, inet_ntoa)
import qualified Network.Transport.TCP as TCP
import qualified Network.Transport.InMemory as Mem

import Network.BSD (getHostName, getHostByName, hostAddress)

import qualified Data.Binary as Binary
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.ByteString.Char8 as BS
import qualified Data.Serialize as S

import qualified System.Timeout
import qualified System.Console.Haskeline as Readline

import DB
import NodeState
import qualified Key
import qualified Utils
import qualified Asset
import qualified Ledger
import qualified Script

import qualified Config
import qualified Account
import qualified Block
import qualified Storage
import qualified NodeState
import qualified Transaction
import qualified Console
import qualified Console.Config
import qualified Logging as Log

import Network.Utils (mkNodeId, extractNodeId)
import Network.P2P.Controller
import Network.P2P.Service (Service(..))
import qualified Network.P2P.Cmd as Cmd
import qualified Network.P2P.Message as Message
import qualified Network.P2P.Consensus as P2PConsensus

-------------------------------------------------------------------------------
-- Initialization
-------------------------------------------------------------------------------

data Mode
  = InMemory
  | TCP
  deriving (Eq, Show)

-- | Starts the peer controller service process
initPeerController
  :: forall m. (MonadReadWriteDB m, MonadProcessBase m)
  => Config.Config
  -> NodeT m ()
initPeerController config = do

  -- Construct bootnode processIds
  cachedPeersNodeIds <- NodeState.peersToNodeIds <$> getCachedPeers
  bootNodeIds <- liftBase $ mapM mkNodeId $ Config.bootnodes config
  let initNodeIds = List.nub $ bootNodeIds ++ cachedPeersNodeIds

  -- Initialize p2p networking controller with known nodes
  peerController initNodeIds

-- | Creates tcp local node which used by 'bootstrap'
createLocalNode
  :: HostName
  -> ServiceName
  -> Maybe RemoteTable
  -> IO DPN.LocalNode
createLocalNode host port mRemoteTable = do
  let transport = TCP
  let rTable = fromMaybe DPN.initRemoteTable mRemoteTable
  case transport of

    InMemory ->  do
      transport <- liftIO $ Mem.createTransport
      DPN.newLocalNode transport rTable

    TCP -> do
      mtransport <- liftIO $
        TCP.createTransport host port TCP.defaultTCPParameters
      case mtransport of
        Left err        -> do
          Log.warning (show err)
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
    +---> logger
    +---> consensus

-}

-- | P2P Service initialization
p2p
  :: (MonadReadWriteDB m, MonadProcessBase m)
  => Config.Config
  -> Chan.Chan Cmd.Cmd -- ^ RPC to P2P channel
  -> (NodeT m () -> Process ())
  -> IO ()
p2p config chan runNodeTtoProcess = do

  -- Create cloud haskell node
  let hostname = toS $ Config.hostname config
  let port     = show $ Config.port config
  let remoteTable = DPN.initRemoteTable
  node <- createLocalNode hostname port (Just remoteTable)

  -- Init Peer controller
  DPN.forkProcess node $ runNodeTtoProcess $
    initPeerController config

  -- Run main process
  DPN.runProcess node $ runNodeTtoProcess $
    waitController 100000 $ mainProcess chan

mainProcess
  :: forall m. (MonadReadWriteDB m, MonadProcessBase m)
  => Chan.Chan Cmd.Cmd
  -> NodeT m ()
mainProcess chan = do

  isTestNode <- NodeState.isTestNode
  let msgService = if isTestNode then TestMessaging else Messaging
  isValidatingNode <- NodeState.isValidatingNode

  -- Unregister old logging process (?)
  unregister (show Logger)

  if isTestNode
    then Log.info "Node is running in test mode."
    else Log.info "Node is running in production mode."

  nodeAddr <- NodeState.askSelfAddress
  Log.info $ "Node account address: " <> show nodeAddr

  -- Spawn process to handle network consensus if validating node
  mConsProc <-
    if isValidatingNode
      then do
        let consProc = do
              Log.info "I am a validating node, according to the genesis block."
              P2PConsensus.consensusProc msgService
        pure $ Just (Consensus, consProc)
      else -- Non validating nodes do not need consensus process
        return Nothing

  -- Construct Process to be supervised
  let procList = maybeToList mConsProc ++        -- Consensus Proc
        [ (Tasks, tasksProc msgService chan)     -- Cmd Proc
        , (msgService, messagingProc msgService) -- Messaging Proc
        , (Logger, lift loggerProc)              -- Logging Proc
        ]

  -- Spawn Supervisor Process
  supervisorPid <- spawnLocal $ supervisorProc procList

  -- Join Network:
  void $ spawnLocal $ do
    -- Wait a few seconds for processes to boot fully
    liftIO $ threadDelay 3000000
    -- Submit CreateAccount Tx to network if new account
    unless isValidatingNode $
      submitNewNodeAccountTx msgService
    -- Join the network by asking for next block
    lastBlockIdx <- Block.index <$> NodeState.getLastBlock
    getBlockAtIdx1Msg <- Message.mkGetBlockAtIdxMsg $ lastBlockIdx + 1
    nsendPeers' msgService getBlockAtIdx1Msg

  -- Don't terminate main process
  forever $ liftIO $
    threadDelay 1000000

-------------------------------------------------------------------------------
-- Supervisor Processes
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

supervisorProc
  :: forall m. MonadProcessBase m
  => [(Service, NodeT m ())]
  -> NodeT m ()
supervisorProc procs = do
    -- Spawn all processes
    procInfos <- mapM (uncurry spawnService) procs
    -- Create MVar for managing live processes
    procsMVar <- liftIO $ newMVar $ Map.fromList procInfos

    -- Wait for ProcessMonitorNotifications or QUIT from cmd line
    controlP $ \runInBase ->
      forever $ receiveWait
        [ match $ runInBase . onMonitorNotif procsMVar
        , match $ runInBase . onKillAllProcs procsMVar
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
          Log.warning $ "[Supervisor] " <> servName <> " died:\n    " <> show died
          -- Fully kill the process
          killService oldPid servInfo
          Log.warning $ "[Supervisor] Cleaning up service: " <> servName
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
  :: MonadProcessBase m
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
          liftIO $ threadDelay 100000
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

-------------------------------------------------------------------------------

-- | Interserver communication from RPC, that can execute Cmd items in the
-- CloudHaskell process.
tasksProc
  :: forall m. (MonadProcessBase m, DB.MonadReadWriteDB m)
  => Service
  -> Chan.Chan Cmd.Cmd
  -> NodeT m ()
tasksProc service chan = do
    spawnLocal rpcHandler
    controlP $ \runInBase ->
      forever $ receiveWait
        [ match $ runInBase . onConsoleMsg ]
  where
    rpcHandler :: NodeT m ()
    rpcHandler =
      forever $ do
        cmd <- liftIO $ Chan.readChan chan
        Log.info $ "Recieved Cmd from RPC:\n\t" <> show cmd

        void $ Cmd.handleCmd service cmd

    onConsoleMsg :: (Cmd.Cmd, SendPort Cmd.CmdResult) -> NodeT m ()
    onConsoleMsg (cmd, sp) = do
      Log.info $ "Recieved Cmd from CLI:\n\t" <> show cmd
      res <- Cmd.handleCmd service cmd
      sendChan sp res

loggerProc :: MonadProcessBase m => m ()
loggerProc = forever $ do
    (time, pid, msg) <- liftP expectLogMsg
    -- Using loging infastructure
    let line = show pid ++ " " ++ msg
    Log.info (toS line)
  where
   expectLogMsg :: Process ([Char], ProcessId, [Char])
   expectLogMsg = expect

-- | Message handling service
-- Expects a bytestring because the canonical serialization library used in this
-- software is Data.Serialize, whereas in order to use cloud haskell's `match`
-- function to receive typed values, a Data.Binary instance must be written.
messagingProc
  :: forall m. (MonadReadWriteDB m, MonadProcessBase m)
  => Service
  -> NodeT m ()
messagingProc service = do
    NodeEnv nodeConfig nodeState <- ask
    controlP $ \runInBase ->
      forever $ receiveWait
        [ match $ runInBase . handleMessage' ]
  where
    handleMessage' :: ByteString -> NodeT m ()
    handleMessage' = Message.handleMessage service

-------------------------------------------------------------------------------

submitNewNodeAccountTx :: MonadProcessBase m => Service -> NodeT m ()
submitNewNodeAccountTx msgService = do
  -- If node is new to network,
  -- submit CreateAccount transaction
  accountType <- NodeState.askAccountType
  case accountType of
    NodeState.Existing -> return ()
    NodeState.New      -> do
      nodeAcc <- NodeState.askAccount
      let pub      = Key.unHexPub $ Key.hexPub $ Account.publicKey nodeAcc
          tz       = Account.timezone nodeAcc
          md       = Account.metadata nodeAcc
          accTxHdr = Transaction.TxAccount $ Transaction.CreateAccount pub tz md
      newAccTx <- Cmd.newTransaction accTxHdr
      Cmd.nsendTransaction msgService newAccTx
