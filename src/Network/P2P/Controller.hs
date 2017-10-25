module Network.P2P.Controller (

  peerController,
  waitController,

  queryAllPeers,
  queryCapablePeers,

  -- ** nsendX using Data.Binary
  nsendPeer,
  nsendPeers,
  nsendPeersAsync,
  nsendPeersMany,
  nsendCapable,
  nsendCapableAsync,

  -- ** nsendX using Data.Serialize
  nsendPeer',
  nsendPeers',
  nsendPeersMany',
  nsendPeersAsync',
  nsendCapable',
  nsendCapableAsync',

  listPeers,
  doDiscover,
  getCachedPeers,
) where

import Protolude hiding (newChan)

import Control.Monad.Base

import Control.Distributed.Process
import Control.Distributed.Process.Serializable (Serializable)

import qualified Data.Set as Set
import qualified Data.Serialize as S

import NodeState
import Network.P2P.Service
import qualified Block
import qualified Logging as Log

-------------------------------------------------------------------------------
-- Peer Controller
-------------------------------------------------------------------------------

-- | A P2P controller service process.
peerController :: [NodeId] -> NodeProcessT ()
peerController seeds = do
  selfPid <- liftBase getSelfPid
  selfAddr <- lift NodeState.askSelfAddress

  -- Add self to list of peers
  setPeers $ Set.singleton $ Peer selfPid selfAddr
  liftBase $ register (show PeerController) selfPid

  -- Discover boot nodes
  liftBase $ mapM_ doDiscover seeds

  nodeConfig <- lift ask
  nodeState <- get
  let runNodeT' = runNodeT nodeConfig nodeState

  Log.info "P2P controller started."
  liftBase $ forever $ receiveWait
    [ matchIf isPeerDiscover $ \reply ->
        runNodeT' $ onDiscover reply
    , match $ \mref ->
        runNodeT' $ onMonitor mref
    , match $ \peerReq ->
        runNodeT' $ onPeerRequest peerReq
    , match $ \peerQuery ->
        runNodeT' $ onPeerQuery peerQuery
    , match $ \peerConfQuery ->
        runNodeT' $ onPeerConfigQuery peerConfQuery
    , match onPeerCapable
    ]

waitController :: Int -> Process a -> Process a
waitController timeout prc = do
  res <- whereis (show PeerController)
  case res of
    Nothing -> do
      let timeleft = timeout - 10000
      if timeleft <= 0
         then Log.critical "Failed to connect" >> terminate
         else do liftIO (threadDelay 10000)
                 waitController timeleft prc
    Just x -> prc

-------------------------------------------------------------------------------
-- Discovery
-------------------------------------------------------------------------------

-- XXX
-- XXX Convert all `Process a` to `NodeProcessT a`
-- XXX

doDiscover :: NodeId -> Process ()
doDiscover node = do
  Log.info $ "Examining node: " <> show node
  whereisRemoteAsync node (show PeerController)

doRegister :: Peer -> NodeProcessT ()
doRegister peer@(Peer pid addr) = do
  isNewPeer <- modifyPeers $ \peers ->
    if Set.member peer peers
      then (peers, False)
      else (Set.insert peer peers, True)
  when isNewPeer $ do
    Log.info $ "Registering peer: " <> show peer
    liftBase $ monitor pid
    Log.info $ "New node with Pid: " <> show (peerPid peer)
    liftBase $ doDiscover $ processNodeId pid

doUnregister :: Maybe MonitorRef -> ProcessId -> NodeProcessT ()
doUnregister mref pid = do
  Log.info $ "Unregistering peer: " <> show pid
  maybe (return ()) (liftBase . unmonitor) mref
  modifyPeers_ $ \peers ->
    flip Set.filter peers $ \peer ->
      peerPid peer /= pid

isPeerDiscover :: WhereIsReply -> Bool
isPeerDiscover (WhereIsReply service pid) =
    service == (show PeerController) && isJust pid

onDiscover :: WhereIsReply -> NodeProcessT ()
onDiscover (WhereIsReply _ Nothing) = return ()
onDiscover (WhereIsReply _ (Just seedPid)) = do
  Log.info $ "Peer discovered: " <> show seedPid
  selfPid <- liftBase getSelfPid
  selfPeer <- Peer selfPid <$> lift NodeState.askSelfAddress
  when (selfPid /= seedPid) $ do
    -- Checks whether peer is in test mode and
    -- if the hash of the genesis block matches
    validPeerConfig <- isPeerConfigValid $ processNodeId seedPid
    when validPeerConfig $ do
      (sp, rp) <- liftBase newChan
      liftBase $ send seedPid (selfPeer, sp :: SendPort Peers)
      Log.info $ "Waiting for peers from " <> show seedPid
      recPeers <- liftBase $ receiveChanTimeout 3000000 rp
      case recPeers of
        Nothing -> Log.info "Failed to find peer."
        Just peers -> do
          known <- getPeers
          let newPeers = Set.difference peers known
          unless (Set.null newPeers) $ do
            Log.info $ "Registering peers..."
            mapM_ doRegister $ newPeers
            let allPeers = Set.union newPeers known
            lift $ cachePeers allPeers

onPeerRequest :: (Peer, SendPort Peers) -> NodeProcessT ()
onPeerRequest (peer, replyTo) = do
  Log.info $ "Peer exchange with " <> show (peerPid peer)
  isNewPeer <- modifyPeers $ \peers ->
    if Set.member peer peers
      then (peers, False)
      else (Set.insert peer peers, True)
  when isNewPeer $ void $
    liftBase $ monitor $ peerPid peer
  liftBase . sendChan replyTo =<< getPeers

onPeerQuery :: SendPort Peers -> NodeProcessT ()
onPeerQuery replyTo = do
  Log.info $ "Local peer query."
  p2pPeers <- getPeers
  liftBase $ sendChan replyTo p2pPeers

onPeerConfigQuery :: SendPort (Bool, ByteString) -> NodeProcessT ()
onPeerConfigQuery replyTo = do
  Log.info $ "Peer config query received."
  testMode <- lift NodeState.isTestNode
  genesisBlock <- lift NodeState.askGenesisBlock
  liftBase $ sendChan replyTo (testMode, Block.hashBlock genesisBlock)

onPeerCapable :: ([Char], SendPort ProcessId) -> Process ()
onPeerCapable (service, replyTo) = do
  Log.info $ "Capability request: " <> show service
  res <- whereis service
  case res of
      Nothing -> Log.info "I can't."
      Just pid -> do
        Log.info "I can!"
        sendChan replyTo pid

onMonitor :: ProcessMonitorNotification -> NodeProcessT ()
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
nsendPeer :: Serializable a => Service -> NodeId -> a -> Process ()
nsendPeer service peer = nsendRemote peer (show service)

-- | Broadcast a message to a specific service on all peers.
-- Blocks on `queryAllPeers` (waits for P2P:Controller to respond)
nsendPeers :: Serializable a => Service -> a -> Process ()
nsendPeers service msg = do
  peers <- queryAllPeers
  Log.info $ "nsendPeers: Sending msg to " <> show (length peers) <> " peers."
  forM_ peers $ \peer -> do
    Log.info $ "Sending msg to " <> (show peer :: Text)
    nsendPeer service peer msg

-- | Broadcast multiple messages to a specific service on all peers.
-- Blocks on `queryAllPeers` (waits for P2P:Controller to respond)
nsendPeersMany :: Serializable a => Service -> [a] -> Process ()
nsendPeersMany service msgs = do
  peers <- queryAllPeers
  Log.info $ "nsendPeers: Sending msg to " <> show (length peers) <> " peers."
  Log.info $ "Sending msg to " <> (show peers :: Text)
  forM_ peers $ \peer -> do
    mapM_ (nsendPeer service peer) msgs

-- | Asynchronous nsendPeers
nsendPeersAsync :: Serializable a => Service -> a -> Process ()
nsendPeersAsync service = void . spawnLocal . nsendPeers service

-- | Broadcast a message to a service of on nodes currently running it.
nsendCapable :: Serializable a => Service -> a -> Process ()
nsendCapable service msg = do
  peers <- queryCapablePeers service
  Log.info $ "nsendCapable: Sending msg to " <> show (length peers) <> " peers."
  forM_ peers $ \peer -> do
    Log.info $ "Sending msg to " <> (show peer :: Text)
    nsendPeer service peer msg

-- | Asynchronous nsendCapable
nsendCapableAsync :: Serializable a => Service -> a -> Process ()
nsendCapableAsync service = void . spawnLocal . nsendCapable service

-------------------------------------------------------------------------------

-- | Like nsendPeer but serialize with Data.Serialize instead of Data.Binary
nsendPeer' :: S.Serialize a => Service -> NodeId -> a -> Process ()
nsendPeer' s p = nsendPeer s p . S.encode

-- | Like nsendPeers but serialize with Data.Serialize instead of Data.Binary
nsendPeers' :: S.Serialize a => Service -> a -> Process ()
nsendPeers' s = nsendPeers s . S.encode

-- | Like nsendPeersMany but serialize with Data.Serialize instead of Data.Binary
nsendPeersMany' :: S.Serialize a => Service -> [a] -> Process ()
nsendPeersMany' s msgs = nsendPeersMany s $ (map S.encode msgs)

-- | Like nsendPeersAsync but serialize with Data.Serialize instead of Data.Binary
nsendPeersAsync' :: S.Serialize a => Service -> a -> Process ()
nsendPeersAsync' s = nsendPeersAsync s . S.encode

-- | Like nsendCapable but serialize with Data.Serialize instead of Data.Binary
nsendCapable' :: S.Serialize a => Service -> a -> Process ()
nsendCapable' s = nsendCapable s . S.encode

-- | Like nsendCapable but serialize with Data.Serialize instead of Data.Binary
nsendCapableAsync' :: S.Serialize a => Service -> a -> Process ()
nsendCapableAsync' s = nsendPeersAsync s . S.encode

-------------------------------------------------------------------------------
-- P2P Discovery Utils
-------------------------------------------------------------------------------

-- | Get a list of currently available peer
-- nodes from local P2P:Controller Process
queryAllPeers :: Process [NodeId]
queryAllPeers = do
  Log.info "Requesting peer list from local controller..."
  (sp, rp) <- newChan
  nsend (show PeerController) (sp :: SendPort Peers)
  peersToNodeIds <$> receiveChan rp

-- | Poll a network for a list of specific service providers.
queryCapablePeers :: Service -> Process [NodeId]
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
queryValidPeerConfig :: NodeId -> Process (Maybe (Bool, ByteString))
queryValidPeerConfig nodeId = do
  (sp, rp) <- newChan
  nsendPeer PeerController nodeId sp
  receiveChanTimeout 3000000 rp

-- | Check to see if peer should join network based on genesis block and test mode
isPeerConfigValid :: NodeId -> NodeProcessT Bool
isPeerConfigValid nodeId = do
  isTestNode <- lift NodeState.isTestNode
  genesisBlockHash <- Block.hashBlock <$> lift NodeState.askGenesisBlock
  mConf <- liftBase $ queryValidPeerConfig nodeId
  case mConf of
    Nothing -> return False
    Just (isPeerTestNode, peerGenBlockHash) -> return $
      and [ isTestNode == isPeerTestNode
          , genesisBlockHash == peerGenBlockHash
          ]

listPeers :: Process ()
listPeers = queryAllPeers >>= (liftIO . print)

cachePeers :: MonadIO m => Peers -> NodeReaderT m ()
cachePeers peers = do
  Log.info "Caching peers..."
  dbRoot <- askDBPath
  liftIO $ savePeers dbRoot peers

getCachedPeers :: MonadIO m => NodeT m Peers
getCachedPeers = liftIO . loadPeers =<< lift askDBPath
