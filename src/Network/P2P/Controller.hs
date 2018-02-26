{-# LANGUAGE ScopedTypeVariables #-}

module Network.P2P.Controller (

  peerControllerProc,

  queryAllPeers,
  queryCapablePeers,

  -- ** nsendX using Data.Binary
  nsendPeer,
  nsendPeers,
  nsendPeersMany,
  nsendCapable,

  -- ** nsendX using Data.Serialize
  nsendPeer',
  nsendPeers',
  nsendPeersMany',
  nsendCapable',

  listPeers,
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

  -- Discover boot nodes
  mapM_ doDiscover initNodeIds

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
doDiscover node = do
    Log.info ("Examining node: " <> show node)
    doDiscover'
  where
    doDiscover' :: m ()
    doDiscover' = do
      (sp,rp) <- newChan
      -- Spawn a discovery process to attempt to discover peer
      pid <- spawnLocal $ discoveryProc sp
      -- But only wait for it to respond for 2 seconds
      mRes <- receiveChanTimeout 2000000 rp
      case mRes of
        Nothing -> Log.warning ("Could not reach node: " <> show node)
        Just _  -> kill pid "Stop discovering peer"

    -- This process is spawned to send a WhereIs message to the potential peer
    -- process. It waits for a WhereIsReply message, and then forward the
    -- message to its parent process, the PeerController process to handle the
    -- message. This is done because wrapping `whereisRemoteAsync` in a a
    -- seemingly simple timeout is actually _not_ simple.
    discoveryProc :: SendPort () -> m ()
    discoveryProc sp = do
      register "discovery" =<< getSelfPid
      -- This might hang for a few minutes...
      whereisRemoteAsync node (show PeerController)
      -- Expect a reply and forward it to the p2p controller proc
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
  selfPeer <- Peer selfPid <$> NodeState.askSelfAddress
  when (selfPid /= seedPid) $ do
    -- Checks whether peer is in test mode and
    -- if the hash of the genesis block matches
    validPeerConfig <- isPeerConfigValid $ processNodeId seedPid
    when validPeerConfig $ do
      (sp, rp) <- liftP newChan
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
  -> m ()
nsendPeer service peer =
  nsendRemote peer (show service)

-- | Broadcast a message to a specific service on all peers.
-- Blocks on `queryAllPeers` (waits for P2P:Controller to respond)
nsendPeers
  :: (MonadProcessBase m, Serializable a)
  => Service
  -> a
  -> m ()
nsendPeers service msg = do
  peers <- queryAllPeers
  Log.info $ "nsendPeers: Sending msg to " <> show (length peers) <> " peers."
  forM_ peers $ \peer -> do
    Log.info $ "Sending msg to " <> (show peer :: Text)
    nsendPeer service peer msg

-- | Broadcast multiple messages to a specific service on all peers.
-- Blocks on `queryAllPeers` (waits for P2P:Controller to respond)
nsendPeersMany
  :: (MonadProcessBase m, Serializable a)
  => Service
  -> [a]
  -> m ()
nsendPeersMany service msgs = do
  peers <- queryAllPeers
  Log.info $ "nsendPeers: Sending msg to " <> show (length peers) <> " peers."
  Log.info $ "Sending msg to " <> (show peers :: Text)
  forM_ peers $ \peer -> do
    mapM_ (nsendPeer service peer) msgs

-- | Broadcast a message to a service of on nodes currently running it.
nsendCapable
  :: (MonadProcessBase m, Serializable a)
  => Service
  -> a
  -> m ()
nsendCapable service msg = do
  peers <- queryCapablePeers service
  Log.info $ "nsendCapable: Sending msg to " <> show (length peers) <> " peers."
  forM_ peers $ \peer -> do
    Log.info $ "Sending msg to " <> (show peer :: Text)
    nsendPeer service peer msg

-------------------------------------------------------------------------------

-- | Like nsendPeer but serialize with Data.Serialize instead of Data.Binary
nsendPeer'
  :: (MonadProcessBase m, S.Serialize a)
  => Service
  -> NodeId
  -> a
  -> m ()
nsendPeer' s p =
  nsendPeer s p . S.encode

-- | Like nsendPeers but serialize with Data.Serialize instead of Data.Binary
nsendPeers'
  :: (MonadProcessBase m, S.Serialize a)
  => Service
  -> a
  -> m ()
nsendPeers' s =
  nsendPeers s . S.encode

-- | Like nsendPeersMany but serialize with Data.Serialize instead of Data.Binary
nsendPeersMany'
  :: (MonadProcessBase m, S.Serialize a)
  => Service
  -> [a]
  -> m ()
nsendPeersMany' s msgs =
  nsendPeersMany s $ (map S.encode msgs)

-- | Like nsendCapable but serialize with Data.Serialize instead of Data.Binary
nsendCapable'
  :: (MonadProcessBase m, S.Serialize a)
  => Service
  -> a
  -> m ()
nsendCapable' s =
  nsendCapable s . S.encode

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

-- | Poll a network for a list of specific service providers.
queryCapablePeers :: MonadProcessBase m => Service -> m [NodeId]
queryCapablePeers service = do
    (sp, rp) <- newChan
    nsendPeers PeerController (service, sp)
    Log.info "Waiting for capable peer nodes..."
    go rp []
  where
    go rp acc = do
      res <- receiveChanTimeout 3000000 rp
      case res of
        Just pid -> do
          Log.info "Found capable peer."
          go rp (processNodeId pid : acc)
        Nothing -> do
          Log.info "Found all capable peers!"
          return acc

-- | Check if peer has valid config
queryValidPeerConfig :: MonadProcessBase m => NodeId -> m (Maybe (Bool, ByteString))
queryValidPeerConfig nodeId = do
  (sp, rp) <- newChan
  nsendPeer PeerController nodeId sp
  receiveChanTimeout 3000000 rp

-- | Check to see if peer should join network based on genesis block and test mode
isPeerConfigValid :: MonadProcessBase m => NodeId -> NodeT m Bool
isPeerConfigValid nodeId = do
  isTestNode <- NodeState.isTestNode
  genesisBlockHash <- Block.hashBlock <$> NodeState.askGenesisBlock
  mConf <- liftP $ queryValidPeerConfig nodeId
  case mConf of
    Nothing -> return False
    Just (isPeerTestNode, peerGenBlockHash) -> return $
      and [ isTestNode == isPeerTestNode
          , genesisBlockHash == peerGenBlockHash
          ]

listPeers :: MonadProcessBase m => m ()
listPeers = queryAllPeers >>= (liftIO . print)

-- XXX --------------------------------------------
-- XXX Rewrite these functions using the general
-- XXX save/loadPeers functions in NodeState
-- XXX --------------------------------------------

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
