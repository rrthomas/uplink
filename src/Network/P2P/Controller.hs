{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.P2P.Controller (

  peerControllerProc,

  doDiscover,
  getCachedPeers,
) where

import Protolude hiding (newChan)

import Control.Monad.Base

import Control.Distributed.Process.Lifted
import Control.Distributed.Process.Lifted.Class

import qualified Data.Binary as B
import qualified Data.List as List
import qualified Data.Set as Set

import NodeState
import Node.Peer
import qualified Network.P2P.Logging as Log
import Network.P2P.Service
import Network.P2P.Send (MatchProcBase(..))
import Network.P2P.SignedMsg as SM
import Network.Utils (mkNodeId)

import qualified Block
import qualified Encoding
import qualified Hash

-------------------------------------------------------------------------------
-- Peer Controller
-------------------------------------------------------------------------------

-- | A P2P controller service process.
peerControllerProc
  :: forall m. MonadProcessBase m
  => [ByteString]
  -> NodeT m ()
peerControllerProc bootnodes = do

  -- Construct bootnode processIds
  bootNodeIds <- mkNodeIds bootnodes

  cachedPeersNodeIds <- Node.Peer.peersToNodeIds <$> getCachedPeers
  let initNodeIds = List.nub $ bootNodeIds ++ cachedPeersNodeIds

  -- Add self to list of peers
  selfPid  <- getSelfPid
  register (show PeerController) selfPid

  selfAddr <- NodeState.askSelfAddress
  setPeers $ Set.singleton $ Peer selfPid selfAddr

  -- Discover boot nodes asynchronously
  mapM_ (void . spawnLocal . doDiscover) initNodeIds

  Log.info "Spawning P2P Controller..."
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
doDiscover :: forall m. MonadProcessBase m => NodeId -> NodeT m ()
doDiscover node = void $ spawnLocal doDiscover'
  where
    -- A process that spawns a discovery process and then waits for 3 seconds
    -- for it to respond that it found a peer or not.
    doDiscover' :: NodeT m ()
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
    discoveryProc :: SendPort () -> NodeT m ()
    discoveryProc sp = do
      -- Warning: This might hang for a few minutes, so we are monitoring it
      -- with `doDiscover'`, and killing it if it takes too long.
      whereisRemoteAsyncSigned node PeerController
      -- Wait for the reply
      eReply <- SM.expectSigned
      case eReply  of
        Left err -> Log.warning $ "Node " <> show node <> " responded with an invalid signature"
        Right (reply :: WhereIsReplySigned) ->
          -- forward response to p2p controller process
          nsendSigned PeerController reply
      -- alert the parent process that we're finished
      sendChan sp ()

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
               Log.info $ "Registering peers..."
               mapM_ doMonitor $ newPeers
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
  Log.info $ "Local peer query."
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
  Log.info $ "Dead node:" <> (show pid)
  doUnmonitor (Just mref) pid
onMonitor (ProcessMonitorNotification mref pid DiedUnknownId) = do
  Log.info $ "Unknown Id:" <> (show pid)
onMonitor (ProcessMonitorNotification mref pid reason) = do
  Log.info $ "Monitor event: " <> show (pid, reason)
  doUnmonitor (Just mref) pid

-------------------------------------------------------------------------------
-- P2P Discovery Utils
-------------------------------------------------------------------------------

data WhereIsSigned = WhereIsSigned ProcessId Service
  deriving (Show, Generic, Typeable, B.Binary)

data WhereIsReplySigned = WhereIsReplySigned Service (Maybe ProcessId)
  deriving (Generic, Typeable, B.Binary)

-- | Implements a signed version of `whereisRemoteAsync`
-- Note: If you use this function to send a `WhereIs` message, the node you are
-- sending it to must be listening for a message of type `WhereIsSigned`. This
-- can be accomplished with `expect >>= onWhereIsSigned`.
whereisRemoteAsyncSigned :: MonadProcessBase m => NodeId -> Service -> NodeT m ()
whereisRemoteAsyncSigned to service = do
  from <- getSelfPid
  nsendPeerSigned PeerController to (WhereIsSigned from service)

onWhereIsSigned :: MonadProcessBase m => WhereIsSigned -> NodeT m ()
onWhereIsSigned (WhereIsSigned from service) = do
  mPid <- whereis (show service)
  let reply = WhereIsReplySigned service mPid
  sendSigned from reply

-- | Check if peer has valid config
queryValidPeerConfig
  :: MonadProcessBase m
  => NodeId
  -> NodeT m (Maybe (Bool, Hash.Hash Encoding.Base16ByteString))
queryValidPeerConfig nodeId = do
  (sp, rp) <- newChan
  nsendPeerSigned PeerController nodeId sp
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
