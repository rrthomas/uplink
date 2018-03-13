{-# LANGUAGE ScopedTypeVariables #-}

module Network.P2P.Controller (

  peerControllerProc,

  queryAllPeers,

  -- ** nsendX using Data.Binary
  nsendPeer,
  nsendPeers,
  nsendPeersMany,

  -- ** nsendX using Data.Serialize
  nsendPeer',
  nsendPeers',
  nsendPeersMany',

  doDiscover,
  getCachedPeers,
) where

import Protolude hiding (newChan)

import Control.Monad.Base

import Control.Distributed.Process.Lifted
import Control.Distributed.Process.Lifted.Class
import Control.Distributed.Process.Serializable (Serializable)

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Serialize as S

import NodeState
import Node.Peer
import qualified Network.P2P.Logging as Log
import Network.P2P.Service
import Network.Utils (waitForLocalService, mkNodeId)

import qualified Block
import qualified Config


-------------------------------------------------------------------------------
-- Peer Controller
-------------------------------------------------------------------------------

-- | A P2P controller service process.
peerControllerProc
  :: forall m. MonadProcessBase m
  => NodeT m ()
peerControllerProc = do

  -- Construct bootnode processIds
  cachedPeersNodeIds <- Node.Peer.peersToNodeIds <$> getCachedPeers
  bootNodeIds <- mkNodeIds . Config.bootnodes =<< askConfig

  let initNodeIds = List.nub $ bootNodeIds ++ cachedPeersNodeIds

  -- Add self to list of peers
  selfPid  <- getSelfPid
  selfAddr <- NodeState.askSelfAddress
  setPeers $ Set.singleton $ Peer selfPid selfAddr
  register (show PeerController) selfPid

  -- Discover boot nodes asynchronously
  mapM_ (void . spawnLocal . doDiscover) initNodeIds

  controlP $ \runInBase -> do
    Log.info "P2P controller started."
    forever $ receiveWait
      [ matchIf isPeerDiscover $ runInBase . onDiscover
      , match $ runInBase . onMonitor
      , match $ runInBase . onPeerRequest
      , match $ runInBase . onPeerQuery
      , match $ runInBase . onPeerConfigQuery
      , match $ runInBase . liftP . onPeerCapable
      ]

  where
    mkNodeIds :: [ByteString] -> NodeT m [NodeId]
    mkNodeIds nodeAddrs =
      fmap concat $
        forM nodeAddrs $ \nodeAddr -> do
          eNodeId <- liftBase $ mkNodeId nodeAddr
          case eNodeId of
            Left err     -> Log.warning err >> pure []
            Right nodeId -> pure [nodeId]

-------------------------------------------------------------------------------
-- Discovery
-------------------------------------------------------------------------------

-- | Attempts to discover a node by sending a WhereIsRemoteAsync message to the remote Node.
--
-- This _should_ be asynchronous, but if the hostname in the NodeId is
-- unreachable, then this function will hang indefinitely trying to establish a
-- connection to the invalid node. To circumvent this behavior, we add a timeout
-- to kill the discovery process if it takes longer than 2 seconds to establish
-- a connection to the given nodeId.
doDiscover :: forall m. MonadProcessBase m => NodeId -> m ()
doDiscover node = void $ spawnLocal doDiscover'
  where
    -- A process that spawns a discovery process and then waits for 3 seconds
    -- for it to respond that it found a peer or not.
    doDiscover' :: m ()
    doDiscover' = do
      register ("discovery-" ++ show node) =<< getSelfPid
      Log.info $ "Examining node: " <> show node
      (sp,rp) <- newChan
      -- Spawn a discovery process to attempt to discover peer
      pid <- spawnLocal $ discoveryProc sp
      -- But only wait for it to respond for 3 seconds
      mRes <- receiveChanTimeout 3000000 rp
      case mRes of
        Nothing -> Log.warning ("Could not reach node: " <> show node)
        Just _  -> pure ()
      kill pid "Stop discovering peer"

    -- This process is spawned to send a WhereIs message to the potential peer
    -- process. It waits for a WhereIsReply message, and then forward the
    -- message to its parent process, the PeerController process to handle the
    -- message. This is done because wrapping `whereisRemoteAsync` in a a
    -- seemingly simple timeout is actually _not_ simple.
    discoveryProc :: SendPort () -> m ()
    discoveryProc sp = do
      -- *** This might hang for a few minutes, so we are monitoring it
      -- with `doDiscover'`, and killing it if it takes too long.
      whereisRemoteAsync node (show PeerController)
      -- Wait for the reply and forward response to p2p controller process
      reply <- expect :: m WhereIsReply
      nsend (show PeerController) reply
      -- alert the parent process that we're finished
      sendChan sp ()

doRegister :: MonadProcessBase m => Peer -> NodeT m ()
doRegister peer@(Peer pid addr) = do
  isNewPeer <- modifyPeers $ \peers ->
    if Set.member peer peers
      then (peers, False)
      else (Set.insert peer peers, True)
  when isNewPeer $ do
    Log.info $ "Registering peer: " <> show peer
    monitor pid
    Log.info $ "New node with Pid: " <> show (peerPid peer)
    doDiscover $ processNodeId pid

doUnregister
  :: MonadProcessBase m
  => Maybe MonitorRef
  -> ProcessId
  -> NodeT m ()
doUnregister mref pid = do
  Log.info $ "Unregistering peer: " <> show pid
  maybe (return ()) (liftP . unmonitor) mref
  modifyPeers_ $ \peers ->
    flip Set.filter peers $ \peer ->
      peerPid peer /= pid

isPeerDiscover :: WhereIsReply -> Bool
isPeerDiscover (WhereIsReply service pid) =
    service == (show PeerController) && isJust pid

onDiscover :: MonadProcessBase m => WhereIsReply -> NodeT m ()
onDiscover (WhereIsReply _ Nothing) = return ()
onDiscover (WhereIsReply _ (Just seedPid)) = do
  Log.info $ "Peer discovered: " <> show seedPid
  selfPid <- liftP getSelfPid
  when (selfPid /= seedPid) $ do
    -- Checks whether peer is in test mode and
    -- if the hash of the genesis block matches
    validPeerConfig <- isPeerConfigValid $ processNodeId seedPid
    when validPeerConfig $ do
      (sp, rp) <- liftP newChan
      selfPeer <- Peer selfPid <$> NodeState.askSelfAddress
      liftP $ send seedPid (selfPeer, sp :: SendPort Peers)
      Log.info $ "Waiting for peers from " <> show seedPid
      recPeers <- liftP $ receiveChanTimeout 3000000 rp
      case recPeers of
        Nothing -> Log.info "Failed to find peer."
        Just peers -> do
          known <- getPeers
          let newPeers = Set.difference peers known
          unless (Set.null newPeers) $ do
            Log.info $ "Registering peers..."
            mapM_ doRegister $ newPeers
            let allPeers = Set.union newPeers known
            cachePeers allPeers

onPeerRequest :: MonadProcessBase m => (Peer, SendPort Peers) -> NodeT m ()
onPeerRequest (peer, replyTo) = do
  Log.info $ "Peer exchange with " <> show (peerPid peer)
  isNewPeer <- modifyPeers $ \peers ->
    if Set.member peer peers
      then (peers, False)
      else (Set.insert peer peers, True)
  when isNewPeer $ void $
    liftP $ monitor $ peerPid peer
  liftP . sendChan replyTo =<< getPeers

onPeerQuery :: MonadProcessBase m => SendPort Peers -> NodeT m ()
onPeerQuery replyTo = do
  Log.info $ "Local peer query."
  p2pPeers <- getPeers
  liftP $ sendChan replyTo p2pPeers

onPeerConfigQuery
  :: MonadProcessBase m
  => SendPort (Bool, ByteString)
  -> NodeT m ()
onPeerConfigQuery replyTo = do
  Log.info $ "Peer config query received."
  testMode <- NodeState.isTestNode
  genesisBlock <- NodeState.askGenesisBlock
  sendChan replyTo (testMode, Block.hashBlock genesisBlock)

onPeerCapable :: ([Char], SendPort ProcessId) -> Process ()
onPeerCapable (service, replyTo) = do
  Log.info $ "Capability request: " <> show service
  res <- whereis service
  case res of
      Nothing -> Log.info "I can't."
      Just pid -> do
        Log.info "I can!"
        sendChan replyTo pid

onMonitor
  :: MonadProcessBase m
  => ProcessMonitorNotification
  -> NodeT m ()
onMonitor (ProcessMonitorNotification mref pid DiedDisconnect) = do
  Log.info $ "Dead node:" <> (show pid)
  doUnregister (Just mref) pid
onMonitor (ProcessMonitorNotification mref pid DiedUnknownId) = do
  Log.info $ "Unknown Id:" <> (show pid)
onMonitor (ProcessMonitorNotification mref pid reason) = do
  Log.info $ "Monitor event: " <> show (pid, reason)
  doUnregister (Just mref) pid

-------------------------------------------------------------------------------
-- P2P Messaging Utils
-------------------------------------------------------------------------------

-- | Send a message to a specific peer process
nsendPeer
  :: (MonadProcessBase m, Serializable a)
  => Service
  -> NodeId
  -> a
  -> NodeT m ()
nsendPeer service peer =
  nsendRemote peer (show service)

-- | Broadcast a message to a specific service on all peers.
nsendPeers
  :: (MonadProcessBase m, Serializable a)
  => Service
  -> a
  -> NodeT m ()
nsendPeers service msg = do
  peers <- getPeerNodeIds
  Log.info $ "nsendPeers: Sending msg to " <> show (length peers) <> " peers."
  forM_ peers $ \peer -> do
    Log.info $ "Sending msg to " <> (show peer :: Text)
    nsendPeer service peer msg

-- | Broadcast multiple messages to a specific service on all peers.
nsendPeersMany
  :: (MonadProcessBase m, Serializable a)
  => Service
  -> [a]
  -> NodeT m ()
nsendPeersMany service msgs = do
  peers <- getPeerNodeIds
  Log.info $ "nsendPeers: Sending msg to " <> show (length peers) <> " peers."
  Log.info $ "Sending msg to " <> (show peers :: Text)
  forM_ peers $ \peer -> do
    mapM_ (nsendPeer service peer) msgs

-------------------------------------------------------------------------------

-- | Like nsendPeer but serialize with Data.Serialize instead of Data.Binary
nsendPeer'
  :: (MonadProcessBase m, S.Serialize a)
  => Service
  -> NodeId
  -> a
  -> NodeT m ()
nsendPeer' s p =
  nsendPeer s p . S.encode

-- | Like nsendPeers but serialize with Data.Serialize instead of Data.Binary
nsendPeers'
  :: (MonadProcessBase m, S.Serialize a)
  => Service
  -> a
  -> NodeT m ()
nsendPeers' s =
  nsendPeers s . S.encode

-- | Like nsendPeersMany but serialize with Data.Serialize instead of Data.Binary
nsendPeersMany'
  :: (MonadProcessBase m, S.Serialize a)
  => Service
  -> [a]
  -> NodeT m ()
nsendPeersMany' s msgs =
  nsendPeersMany s $ (map S.encode msgs)

-------------------------------------------------------------------------------
-- P2P Discovery Utils
-------------------------------------------------------------------------------

-- | Get a list of currently available peer
-- nodes from local P2P:Controller Process
queryAllPeers :: MonadProcessBase m => m [NodeId]
queryAllPeers = do
  Log.info "Requesting peer list from local controller..."
  (sp, rp) <- newChan
  nsend (show PeerController) (sp :: SendPort Peers)
  peersToNodeIds <$> receiveChan rp

-- | Check if peer has valid config
queryValidPeerConfig
  :: MonadProcessBase m
  => NodeId
  -> NodeT m (Maybe (Bool, ByteString))
queryValidPeerConfig nodeId = do
  (sp, rp) <- newChan
  nsendPeer PeerController nodeId sp
  receiveChanTimeout 3000000 rp

-- | Check to see if peer should join network based on genesis block and test mode
isPeerConfigValid :: MonadProcessBase m => NodeId -> NodeT m Bool
isPeerConfigValid nodeId = do
  isTestNode <- NodeState.isTestNode
  genesisBlockHash <- Block.hashBlock <$> NodeState.askGenesisBlock
  mConf <- queryValidPeerConfig nodeId
  case mConf of
    Nothing -> return False
    Just (isPeerTestNode, peerGenBlockHash) -> return $
      and [ isTestNode == isPeerTestNode
          , genesisBlockHash == peerGenBlockHash
          ]

----------------------------------------------
-- Cached Peers
----------------------------------------------

cachePeers :: (MonadProcess m) => Peers -> NodeT m ()
cachePeers peers = do
  Log.info "Caching peers..."
  peersFilePath <- NodeState.askPeersFilePath
  liftBase $ writePeers' peersFilePath peers

getCachedPeers :: (MonadProcess m) => NodeT m Peers
getCachedPeers = do
  ePeers <- getCachedPeers'
  case ePeers of
    Left err -> do
      Log.warning $ "Failed to load cached peers: " <> err
      pure Set.empty
    Right peers -> pure peers

getCachedPeers' :: MonadBase IO m => NodeT m (Either Text Peers)
getCachedPeers' = do
  peersFilePath <- NodeState.askPeersFilePath
  liftBase $ readPeers' peersFilePath
