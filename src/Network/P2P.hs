{-|

Peer to peer network interface.

-}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Network.P2P (
  -- ** Server
  p2p,
  createLocalNode
) where

import Protolude hiding (newChan, catch)

import Control.Monad.Base (liftBase)
import Control.Monad.Catch (catch)
import Control.Distributed.Process as DP hiding (catch)
import Control.Distributed.Process.Serializable (Serializable)
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Distributed.Process.Node as DPN

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

import NodeState
import qualified Key
import qualified Utils
import qualified Asset
import qualified Ledger
import qualified Script

import qualified Config
import qualified Account
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

-- | Start a controller service process and aquire connections to a swarm.
bootstrap
  :: RemoteTable
  -> NodeConfig
  -> NodeState
  -> NodeProcessT ()
  -> IO ()
bootstrap rTable nodeConfig nodeState mainProc = do
  let config = NodeState.config nodeConfig

  let hostname = Config.hostname config
  node <- createLocalNode hostname (show $ Config.port config) rTable

  Log.info $ toS $ "Node Hostname: " ++ hostname

  cachedPeersNodeIds <- runNodeT nodeConfig nodeState $ NodeState.peersToNodeIds <$> getCachedPeers
  bootNodeIds <- mapM mkNodeId $ Config.bootnodes config
  let initNodeIds = List.nub $ bootNodeIds ++ cachedPeersNodeIds
  -- Initialize p2p networking controller with known nodes
  void $ DPN.forkProcess node $
    runNodeT nodeConfig nodeState $
      peerController initNodeIds

    -- Run main Process
  DPN.runProcess node $
    waitController 100000 $
      runNodeT nodeConfig nodeState mainProc

resolveHostName :: HostName -> IO HostName
resolveHostName = inet_ntoa . hostAddress <=< getHostByName

-- | Creates tcp local node which used by 'bootstrap'
createLocalNode :: HostName -> ServiceName -> RemoteTable -> IO DPN.LocalNode
createLocalNode host port rTable = do
  let transport = TCP -- XXX
  case transport of

    InMemory ->  do
      transport <- Mem.createTransport
      DPN.newLocalNode transport rTable

    TCP -> do
      mtransport <- TCP.createTransport host port TCP.defaultTCPParameters
      case mtransport of
        Left err        -> do
          Log.warning (show err)
          Utils.dieRed "Could not bind socket on port. Now I die..."
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
  :: Chan.Chan Cmd.Cmd -- ^ RPC to P2P channel
  -> NodeConfig        -- ^ Node Config (Read only)
  -> NodeState         -- ^ Mutable NodeState
  -> IO ()
p2p chan nodeConfig nodeState = do
  let main = mainProcess chan
  bootstrap DPN.initRemoteTable nodeConfig nodeState main

mainProcess
  :: Chan.Chan Cmd.Cmd
  -> NodeProcessT ()
mainProcess chan = do

  isTestNode <- lift NodeState.isTestNode
  let msgService = if isTestNode then TestMessaging else Messaging
  isValidatingNode <- NodeState.isValidatingNode

  -- Unregister old logging process (?)
  liftBase $ unregister (show Logger)

  if isTestNode
    then Log.info "Node is running in test mode."
    else Log.info "Node is running in production mode."

  nodeAddr <- lift NodeState.askSelfAddress
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
  let procList = maybeToList mConsProc ++                 -- Consensus Proc
        [ (Tasks, tasksProc msgService chan)     -- Cmd Proc
        , (msgService, messagingProc msgService) -- Messaging Proc
        , (Logger, liftBase loggerProc)          -- Logging Proc
        ]

  -- Spawn Supervisor Process
  supervisorPid <- spawnLocalT $ supervisorProc procList

  -- Join Network:
  void $ spawnLocalT $ do
    -- Wait a few seconds for processes to boot fully
    liftIO $ threadDelay 3000000
    -- Submit CreateAccount Tx to network if new account
    unless isValidatingNode $
      submitNewNodeAccountTx msgService
    -- Join the network by asking for block 1
    getBlockAtIdx1Msg <- Message.mkGetBlockAtIdxMsg 1
    liftBase $ nsendPeers' msgService getBlockAtIdx1Msg

  -- Don't terminate main process
  forever $ liftIO $
    threadDelay 1000000

  -- Spawn shell for interactive testing
  -- config <- lift NodeState.askConfig
  -- unless (Config.noConsole config) $
  --   consoleProc chan

-------------------------------------------------------------------------------
-- Supervisor Processes
-------------------------------------------------------------------------------

data ServiceInfo = ServiceInfo
  { service    :: Service
  , monitorRef :: MonitorRef
  , process    :: NodeProcessT ()
  }

type ProcessMap = Map ProcessId ServiceInfo

data KillAllProcs = KillAllProcs
  deriving (Generic, Binary.Binary)

supervisorProc :: [(Service, NodeProcessT ())] -> NodeProcessT ()
supervisorProc procs = do
    -- Spawn all processes
    procInfos <- mapM (uncurry spawnService) procs
    -- Create MVar for managing live processes
    procsMVar <- liftIO $ newMVar $ Map.fromList procInfos

    nodeConfig <- lift ask
    nodeState <- get
    -- Wait for ProcessMonitorNotifications or QUIT from cmd line
    liftBase $ forever $ receiveWait
      [ match $ \procMonNotif ->
          runNodeT nodeConfig nodeState $
            onMonitorNotif procsMVar procMonNotif
      , match $ onKillAllProcs procsMVar
      ]
  where
    onMonitorNotif
      :: MVar ProcessMap
      -> ProcessMonitorNotification
      -> NodeProcessT ()
    onMonitorNotif procsMVar (ProcessMonitorNotification mref oldPid _) = do
      procMap <- liftIO $ readMVar procsMVar
      case Map.lookup oldPid procMap of
        Nothing -> do
          Log.warning $
            "[Supervisor] Unrecognized processId on monitor: " <> show oldPid
          liftBase $ unmonitor mref -- unmonitor the unrecognized process
        Just servInfo@(ServiceInfo s mref proc) -> do
          let servName = show s
          Log.warning $ "[Supervisor] " <> servName <> " died..."
          -- Fully kill the process
          liftBase $ killService oldPid servInfo
          Log.warning $ "[Supervisor] Cleaning up service: " <> servName
          -- Start new instance of same process
          (newPid, newServInfo) <- spawnService s proc
          Log.warning $ "[Supervisor] Spawned new service: " <> servName
          -- Update the managed processes MVar to reflect new process
          liftIO $ modifyMVar_ procsMVar $ \procMap' ->
            pure $ Map.insert newPid newServInfo $ Map.delete oldPid procMap'

    onKillAllProcs :: MVar ProcessMap -> KillAllProcs -> Process ()
    onKillAllProcs procsMVar _ = do
      procs <- liftIO $ readMVar procsMVar
      mapM_ (uncurry killService) $ Map.toList procs

-- | Spawn a process, registering it with the given name and
-- monitoring it. The child process spawned will report
-- `ProcessMonitorNotification`s to the process that calls this function.
spawnService
  :: Service
  -> NodeProcessT ()
  -> NodeProcessT (ProcessId, ServiceInfo)
spawnService service nproc = do
  Log.info $ "Spawning service: " <> show service
  pid <- spawnLocalT nproc
  mref <- liftBase $ do
    isAlive <- whereis (show service)
    case isAlive of
      Nothing  -> register (show service) pid
      Just _ -> reregister (show service) pid
    monitor pid
  pure (pid, ServiceInfo service mref nproc)

-- | Cleanup from a spawnService call
killService :: ProcessId -> ServiceInfo -> Process ()
killService pid (ServiceInfo service mref _) = do
  let serviceName = show service
  let killMsg = "Killing service: " <> serviceName
  Log.warning killMsg
  kill pid $ toS killMsg
  unmonitor mref

-------------------------------------------------------------------------------

-- | Interserver communication from RPC, that can execute Cmd items in the
-- CloudHaskell process.
tasksProc
  :: Service
  -> Chan.Chan Cmd.Cmd
  -> NodeProcessT ()
tasksProc service chan = do
    nodeConfig <- ask
    nodeState <- get
    liftBase $ spawnLocal $ rpcHandler nodeConfig nodeState
    forever $
       liftBase $ receiveWait [ match (onConsoleMsg nodeConfig nodeState) ]
  where
    rpcHandler :: NodeConfig -> NodeState -> Process ()
    rpcHandler nodeConfig nodeState =
      forever $ do
        cmd <- liftIO $ Chan.readChan chan
        Log.info $ "Recieved Cmd from RPC:\n\t" <> show cmd

        NodeState.runNodeT nodeConfig nodeState $ Cmd.handleCmd service cmd
        return ()

    onConsoleMsg :: NodeConfig -> NodeState -> (Cmd.Cmd, SendPort Cmd.CmdResult) -> Process ()
    onConsoleMsg nodeConfig nodeState (cmd, sp) = do
      Log.info $ "Recieved Cmd from CLI:\n\t" <> show cmd
      res <- NodeState.runNodeT nodeConfig nodeState $ Cmd.handleCmd service cmd
      sendChan sp res

loggerProc :: Process ()
loggerProc =
  forever $ do
    (time, pid, msg) <- expect :: Process ([Char], ProcessId, [Char])
    -- Using loging infastructure
    let line = show pid ++ " " ++ msg
    Log.info (toS line)

-- | Message handling service
-- Expects a bytestring because the canonical serialization library used in this
-- software is Data.Serialize, whereas in order to use cloud haskell's `match`
-- function to receive typed values, a Data.Binary instance must be written.
messagingProc :: Service -> NodeProcessT ()
messagingProc service = do
    nodeConfig <- lift ask
    nodeState <- get
    liftBase $ forever $ receiveWait
      [ match $ runNodeT nodeConfig nodeState . handleMessage' ]
  where
    handleMessage' :: ByteString -> NodeProcessT ()
    handleMessage' = Message.handleMessage service

-------------------------------------------------------------------------------

submitNewNodeAccountTx :: Service -> NodeProcessT ()
submitNewNodeAccountTx msgService = do
  -- If node is new to network,
  -- submit CreateAccount transaction
  accountType <- lift NodeState.askAccountType
  case accountType of
    NodeState.Existing -> return ()
    NodeState.New      -> do
      nodeAcc <- lift NodeState.askAccount
      let pub      = Key.unHexPub $ Key.hexPub $ Account.publicKey nodeAcc
          tz       = Account.timezone nodeAcc
          md       = Account.metadata nodeAcc
          accTxHdr = Transaction.TxAccount $ Transaction.CreateAccount pub tz md
      newAccTx <- Cmd.newTransaction accTxHdr
      liftBase $ Cmd.nsendTransaction msgService newAccTx
