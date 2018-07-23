{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Network.P2P.Controller (

  PeerController(..),

  doDiscover,
  getCachedPeers,
) where

import Protolude hiding (newChan, link)

import Control.Monad.Base

import Control.Distributed.Process.Lifted
import Control.Distributed.Process.Lifted.Class

import qualified Data.Binary as B
import qualified Data.List as List
import qualified Data.Set as Set

import NodeState
import Node.Peer
import qualified Network.P2P.Logging as Log
import qualified Network.P2P.Logging.Rule as Log
import Network.P2P.Service
import Network.P2P.Send (MatchProcBase(..))
import Network.P2P.SignedMsg as SM
import Network.Utils (mkNodeId)

import qualified Block
import qualified Encoding
import qualified Hash

-------------------------------------------------------------------------------
-- Peer Controller Service Definition
-------------------------------------------------------------------------------

data PeerController = PeerController
  deriving (Show, Generic, B.Binary)

instance Service PeerController where
  serviceSpec PeerController = Worker peerControllerProc

-------------------------------------------------------------------------------
-- Peer Controller Process
-------------------------------------------------------------------------------

-- | A P2P controller service process.
peerControllerProc
  :: MonadProcessBase m
  => NodeT m ()
peerControllerProc = do

  -- Construct bootnode processIds
  bootNodeIds <- bootnodes <$> askNodeConfig

  cachedPeersNodeIds <- Node.Peer.peersToNodeIds <$> getCachedPeers
  let initNodeIds = List.nub $ bootNodeIds ++ cachedPeersNodeIds

  -- Add self to list of peers
  selfPid  <- getSelfPid
  selfAddr <- NodeState.askSelfAddress
  setPeers $ Set.singleton $ Peer selfPid selfAddr

  -- Discover boot nodes asynchronously
  mapM_ (void . spawnLocal . doDiscover) initNodeIds

  forever $
    SM.receiveWaitSigned
      [ MatchProcBase onWhereIsSigned
      , MatchProcBase onDiscover
      , MatchProcBase onMonitor
      , MatchProcBase onPeerRequest
      , MatchProcBase onPeerQuery
      , MatchProcBase onPeerConfigQuery
      ]

  where
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

-- | Go ahead and send the log message to the Logging process _pretending_ to be
-- the 'PeerController' process.
logDiscoverMsg :: MonadProcessBase m => Log.Severity -> Text -> m ()
logDiscoverMsg sev msg  = do
  let logMsg = Log.LogMsg (Just (show PeerController)) sev msg
  nsend (show Log.Logger) logMsg

-- | Attempts to discover a node by sending a WhereIsRemoteAsync message to the remote Node.
--
-- This _should_ be asynchronous, but if the hostname in the NodeId is
-- unreachable, then this function will hang indefinitely trying to establish a
-- connection to the invalid node. To circumvent this behavior, we add a timeout
-- to kill the discovery process if it takes longer than 2 seconds to establish
-- a connection to the given nodeId.
doDiscover :: forall m. MonadProcessBase m => NodeId -> NodeT m ()
doDiscover node = do
    selfPid <- getSelfPid
    logDiscoverMsg Log.Info $ "Examining node: " <> show node
    -- Spawn a discovery process to attempt to discover peer
    discoveryProcPid <- spawnLocal $ discoveryProc selfPid
    discProcMRef <- monitor discoveryProcPid
    -- But only wait for it to respond for 3 seconds
    mRes <- receiveTimeout 3000000
      [ match $ \() -> pure ()
      , match onMonitorDiscovery
      ]
    case mRes of
      Nothing -> logDiscoverMsg Log.Warning $ "Could not reach node: " <> show node
      Just _  -> pure ()
    unmonitor discProcMRef
    kill discoveryProcPid "Stop discovering peer"

  where

    -- This process is spawned to send a WhereIs message to the potential peer
    -- process. It waits for a WhereIsReply message, and then forward the
    -- message to its parent process, the PeerController process to handle the
    -- message. This is done because wrapping `whereisRemoteAsync` in a a
    -- seemingly simple timeout is actually _not_ simple.
    discoveryProc :: ProcessId -> NodeT m ()
    discoveryProc parentPid = do
      link parentPid
      -- Warning: This might hang for a few minutes, so we are monitoring it
      -- with `doDiscover'`, and killing it if it takes too long.
      whereisRemoteAsyncSigned node PeerController
      -- Wait for the reply
      eReply <- SM.expectSigned
      case eReply  of
        Left err -> logDiscoverMsg Log.Warning $
          "Node " <> show node <> " responded with an invalid signature"
        Right (reply :: WhereIsReplySigned) ->
          -- forward response to p2p controller process
          nsendSigned PeerController reply
      -- alert the parent process that we're finished
      send parentPid ()

    onMonitorDiscovery (ProcessMonitorNotification mref pid reason) = do
      logDiscoverMsg Log.Warning $ "Monitor notification from discovery proc: " <> show (pid, reason)

-- | Monitor a specific process such that when it dies the monitoring process
-- receives a ProcessMonitorNotification
doMonitor :: MonadProcessBase m => Peer -> NodeT m ()
doMonitor peer@(Peer pid addr) = do
  isNewPeer <- modifyPeers $ \peers ->
    if Set.member peer peers
      then (peers, False)
      else (Set.insert peer peers, True)
  when isNewPeer $ do
    let peerNodeId = processNodeId pid
    Log.info $ "Beginning to monitor peer: " <> show peerNodeId
    monitor pid
    doDiscover peerNodeId

-- | Unmonitor a process such that if it dies no notification is received.
doUnmonitor
  :: MonadProcessBase m
  => Maybe MonitorRef
  -> ProcessId
  -> NodeT m ()
doUnmonitor mref pid = do
  Log.info $ "Unmonitoring peer: " <> show pid
  maybe (return ()) (liftP . unmonitor) mref
  modifyPeers_ $ \peers ->
    flip Set.filter peers $ \peer ->
      peerPid peer /= pid

onDiscover :: MonadProcessBase m => WhereIsReplySigned -> NodeT m ()
onDiscover (WhereIsReplySigned _ Nothing) = pure ()
onDiscover (WhereIsReplySigned _ (Just peerPid)) = do
  Log.info $ "Peer discovered: " <> show peerPid
  selfPid <- getSelfPid
  when (selfPid /= peerPid) $ do
    -- Checks whether peer is in test mode and
    -- if the hash of the genesis block matches
    let peerNodeId = processNodeId peerPid
    validPeerConfig <- isPeerConfigValid peerNodeId
    if validPeerConfig
       then do
         Log.info $ "*** Successful peer config query " <> show peerNodeId <> " ***"
         (sp, rp) <- newChan
         selfPeer <- Peer selfPid <$> NodeState.askSelfAddress
         sendSigned peerPid (selfPeer, sp :: SM.SendPortSigned Peers)
         Log.info $ "Waiting for peers from " <> show peerPid
         recPeers <- SM.receiveChanTimeoutSigned 3000000 rp
         case recPeers of
           Nothing            -> Log.info "Failed to find peer."
           Just (Left err)    -> Log.warning $ show err <> " from " <> show peerPid
           Just (Right peers) -> do
             known <- getPeers
             let newPeers = Set.difference peers known
             unless (Set.null newPeers) $ do
               Log.info "Registering peers..."
               mapM_ doMonitor newPeers
               let allPeers = Set.union newPeers known
               cachePeers allPeers
        else
          Log.warning $ "Invalid peer config: " <> show peerNodeId

onPeerRequest :: MonadProcessBase m => (Peer, SM.SendPortSigned Peers) -> NodeT m ()
onPeerRequest (peer, replyTo) = do
  Log.info $ "Peer exchange with " <> show (peerPid peer)
  isNewPeer <- modifyPeers $ \peers ->
    if Set.member peer peers
      then (peers, False)
      else (Set.insert peer peers, True)
  when isNewPeer $ void $
    monitor $ peerPid peer
  SM.sendChanSigned replyTo =<< getPeers

onPeerQuery :: MonadProcessBase m => SendPortSigned Peers -> NodeT m ()
onPeerQuery replyTo = do
  Log.info "Local peer query."
  p2pPeers <- getPeers
  SM.sendChanSigned replyTo p2pPeers

onPeerConfigQuery
  :: MonadProcessBase m
  => SendPortSigned (Bool, Hash.Hash Encoding.Base16ByteString)
  -> NodeT m ()
onPeerConfigQuery replyTo = do
  Log.info $ "Peer config query received."
  testMode <- NodeState.isTestNode
  genesisBlock <- NodeState.askGenesisBlock
  SM.sendChanSigned replyTo (testMode, Block.hashBlock genesisBlock)

onMonitor
  :: MonadProcessBase m
  => ProcessMonitorNotification
  -> NodeT m ()
onMonitor (ProcessMonitorNotification mref pid DiedDisconnect) = do
  Log.info $ "Process disconnected:" <> (show pid)
  doUnmonitor (Just mref) pid
onMonitor (ProcessMonitorNotification mref pid DiedUnknownId) = do
  Log.info $ "Unknown Id:" <> (show pid)
onMonitor (ProcessMonitorNotification mref pid reason) = do
  Log.info $ "Monitor event: " <> show (pid, reason)
  doUnmonitor (Just mref) pid

-------------------------------------------------------------------------------
-- P2P Discovery Utils
-------------------------------------------------------------------------------

data WhereIsSigned = WhereIsSigned ProcessId [Char]
  deriving (Show, Generic, Typeable, B.Binary)

data WhereIsReplySigned = WhereIsReplySigned [Char] (Maybe ProcessId)
  deriving (Show, Generic, Typeable, B.Binary)

-- | Implements a signed version of `whereisRemoteAsync`
-- Note: If you use this function to send a `WhereIs` message, the node you are
-- sending it to must be listening for a message of type `WhereIsSigned`. This
-- can be accomplished with `expect >>= onWhereIsSigned`
whereisRemoteAsyncSigned :: (MonadProcessBase m, Service s) => NodeId -> s -> NodeT m ()
whereisRemoteAsyncSigned to service = do
  from <- getSelfPid
  nsendPeerSigned to PeerController (WhereIsSigned from (show service))

onWhereIsSigned :: MonadProcessBase m => WhereIsSigned -> NodeT m ()
onWhereIsSigned (WhereIsSigned from service) = do
  mPid <- whereis service
  let reply = WhereIsReplySigned service mPid
  sendSigned from reply

-- | Check if peer has valid config
queryValidPeerConfig
  :: MonadProcessBase m
  => NodeId
  -> NodeT m (Maybe (Bool, Hash.Hash Encoding.Base16ByteString))
queryValidPeerConfig nodeId = do
  (sp, rp) <- newChan
  nsendPeerSigned nodeId PeerController sp
  mResp <- receiveChanTimeoutSigned 3000000 rp
  case mResp of
    Nothing -> pure Nothing
    Just (Left err) -> do
      Log.warning $ show err <> " from " <> show nodeId
      pure Nothing
    Just (Right resp) -> pure (Just resp)

-- | Check to see if peer should join network based on genesis block and test mode
isPeerConfigValid :: MonadProcessBase m => NodeId -> NodeT m Bool
isPeerConfigValid nodeId = do
  isTestNode <- NodeState.isTestNode
  genesisBlockHash <- Block.hashBlock <$> NodeState.askGenesisBlock
  mConf <- queryValidPeerConfig nodeId
  case mConf of
    Nothing -> do
      selfNodeId <- liftP getSelfNode
      Log.warning $ "*** Timed out on peer config query: " <> show selfNodeId <> " ***"
      return False
    Just (isPeerTestNode, peerGenBlockHash) -> do
      return $
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
