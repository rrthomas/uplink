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
  Uplink(..),

  runUplink,

  createLocalNodeFromService,
  createLocalNodeFromService',
  createLocalNode,
  createLocalNode',
  createLocalNodeWithTransport,
  createLocalNodeWithTransport'
) where

import Protolude hiding (newChan, catch)

import Control.Distributed.Process.Lifted
import Control.Distributed.Process.Lifted.Class
import qualified Control.Distributed.Process.Node as DPN

import Network.Socket (HostName, ServiceName)
import qualified Network.Transport.TCP as TCP
import qualified Network.Transport.TCP.TLS as TLS
import qualified Network.Transport.InMemory as Mem
import qualified Consensus.Authority.Params as CAP
import qualified Network.Transport as NT

import qualified Data.Binary as Binary

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
import qualified Network.P2P.Consensus as Con
import qualified Network.P2P.Simulate as Sim
import Network.P2P.Controller
import Network.P2P.SignedMsg (nsendPeersSigned)
import qualified Network.P2P.Logging as Log
import qualified Network.P2P.Message as Msg
import Network.P2P.Service
         ( Service(..), ServiceSpec(..), HList(..)
         , waitLocalService, waitLocalProc', serviceToProc
         )

-------------------------------------------------------------------------------
-- Uplink Service Definition
-------------------------------------------------------------------------------

data Uplink = Uplink
  deriving (Show, Generic, Binary.Binary)

instance Service Uplink where
  serviceSpec _ =
    Supervisor $ PeerController
             ::: Cmd.ExternalCmd
             ::: Msg.Messaging
             ::: Sim.Simulation
             ::: Con.Consensus
             ::: HNil

-------------------------------------------------------------------------------
-- Node Creation
-------------------------------------------------------------------------------

-- | Create a LocalNode from a given service definition. This function also
-- reifys the service from a 'NodeT m ()' to a 'Process ()' value and returns it
createLocalNodeFromService
  :: (MonadProcessBase m, MonadReadWriteDB m, Service s)
  => Config.Config
  -> s
  -> (NodeT m () -> Process ())
  -> IO (DPN.LocalNode, Process ())
createLocalNodeFromService Config.Config{..} s run = do
   (localNode, proc, _) <-
     createLocalNodeFromService' transport hostname (show port) s run
   pure (localNode, proc)

-- | The same as 'createLocalNodeFromService' but returns the transport created
-- for testing purposes.
createLocalNodeFromService'
  :: (MonadProcessBase m, MonadReadWriteDB m, Service s)
  => Config.Transport
  -> HostName
  -> ServiceName
  -> s                          -- ^ Service to create remote table with
  -> (NodeT m () -> Process ()) -- ^ Runner function
  -> IO (DPN.LocalNode, Process (), NT.Transport)
createLocalNodeFromService' t hn p s run = do
    (localNode, tp) <- createLocalNodeWithTransport' t hn p (Just remoteTable)
    pure (localNode, proc, tp)
  where
    remoteTable = mkRemoteTable DPN.initRemoteTable
    (proc, mkRemoteTable) = serviceToProc run s

-- | Creates a local TCP cloud haskell node using from Config
createLocalNode
  :: Config.Config
  -> Maybe (RemoteTable -> RemoteTable)
  -> IO DPN.LocalNode
createLocalNode config mMkRemoteTable = do
    createLocalNode' (Config.transport config) hostname port mRemoteTable
  where
    hostname = toS $ Config.hostname config
    port     = show $ Config.port config
    mRemoteTable = mMkRemoteTable <*> pure DPN.initRemoteTable

-- | Creates a local TCP cloud haskell node
createLocalNode'
  :: Config.Transport
  -> HostName
  -> ServiceName
  -> Maybe RemoteTable
  -> IO DPN.LocalNode
createLocalNode' cfgTransport host port remoteTable =
  fst <$> createLocalNodeWithTransport' cfgTransport host port remoteTable

-- | Creates a local TCP cloud haskell node.
-- Returns Network Transport for testing purposes
-- We use the returned Network Transport value to shutdown the transport
createLocalNodeWithTransport'
  :: Config.Transport
  -> HostName
  -> ServiceName
  -> Maybe RemoteTable
  -> IO (DPN.LocalNode, NT.Transport)
createLocalNodeWithTransport' cfgTransport host port mRemoteTable = do
  let rTable = fromMaybe DPN.initRemoteTable mRemoteTable
  case cfgTransport of

    Config.InMemory ->  do
      transport <- liftIO Mem.createTransport
      (, transport) <$> DPN.newLocalNode transport rTable

    Config.TCP -> do
      mtransport <- TCP.createTransport host port (host,) TCP.defaultTCPParameters
      case mtransport of
        Left err        -> do
          liftIO $ Utils.dieRed $ show err
          liftIO $ Utils.dieRed $ "Could not bind socket on port: " <> toS port <> " Now I die..."
        Right transport -> do
          liftIO $ Utils.putGreen "Creating new local node"
          (, transport) <$> DPN.newLocalNode transport rTable

    Config.TLS Config.TLSConfig{..} -> do
      eTlsConfig <- TLS.mkTLSConfig TLS.defaultSupported (serverCertFp, serverKeyFp) mCertStoreFp Nothing
      case eTlsConfig of
        Left err -> do
          liftIO $ Utils.dieRed $ show err
          liftIO $ Utils.dieRed ("createLocalNode': " <> show err)
        Right tlsConfig -> do
          let tcpAddr = TLS.defaultTCPAddr host port
          eTransport <- TLS.createTransport tcpAddr (TLS.defaultTCPParameters tlsConfig)
          case eTransport of
            Left err        -> do
              liftIO $ Utils.dieRed $ show err
              liftIO $ Utils.dieRed $ "Could not bind socket on port: " <> toS port <> " Now I die..."
            Right transport ->
              (, transport) <$> DPN.newLocalNode transport rTable

-- | Creates a local TCP cloud haskell node.
-- Returns Network Transport for testing purposes
-- We use the returned Network Transport value to shutdown the transport
createLocalNodeWithTransport :: Config.Config -> IO (DPN.LocalNode, NT.Transport)
createLocalNodeWithTransport config =
    createLocalNodeWithTransport' (Config.transport config) hostname port (Just DPN.initRemoteTable)
  where
    hostname = toS $ Config.hostname config
    port     = show $ Config.port config

-------------------------------------------------------------------------------
-- Node
-------------------------------------------------------------------------------

{- |

 Processes
 =========

  Uplink Node             Console --|-- RPC
    |                               |
    +---> PeerController            |
    +---> Messaging                 |
    +---> ExternalCmd      <--------+
    +---> Consensus
              |
              +----> ConsensusBlockGen
              +----> ConsensusMessaging


-}

-- | Uplink P2P Service initialization
runUplink
  :: forall m. (MonadReadWriteDB m, MonadProcessBase m)
  => Process ()
  -> NodeT m ()
runUplink servicesProc = do

  register "Main" =<< getSelfPid

  -- Spawn the Logging process, the only unsupervised process
  lpid <- spawnLocal $ do
    register (show Log.Logger) =<< getSelfPid
    Log.loggerProc . loggerRules =<< askNodeConfig

  -- Wait for Logging process to spawn before booting the rest of Uplink
  waitLocalProc' (show Log.Logger) 1000000 $ do

    isTestNode <- NodeState.isTestNode

    if isTestNode
      then Log.info "Node is running in test mode."
      else Log.info "Node is running in production mode."

    nodeAddr <- NodeState.askSelfAddress
    Log.info $ "Node account address: " <> show nodeAddr

    -- Spawn the Uplink Process tree
    spawnLocal (liftP servicesProc)

    -- Join Network after waiting for Messaging proc to spawn:
    Log.info "Joining the Uplink network..."
    mres <- waitLocalService Msg.Messaging 3000000 $ do
      -- Submit CreateAccount Tx to network if new account
      isValidatingNode <- NodeState.isValidatingNode
      when isValidatingNode $
        Log.info "I am a validating node, according to the genesis block."

      joinNetwork

      -- Join the network by asking for next block
      lastBlockIdx <- Block.index <$> NodeState.getLastBlock
      getBlockAtIdx1Msg <- Msg.mkGetBlockAtIdxMsg $ lastBlockIdx + 1
      nsendPeersSigned Msg.Messaging getBlockAtIdx1Msg

    case mres of
      Nothing -> do
        Log.critical "Failed to join network. Terminating."
        terminate
      Just _  ->
        -- Don't terminate main process
        forever $ liftIO $
          threadDelay 1000000

  where
    joinNetwork :: NodeT m ()
    joinNetwork = do
      peers <- NodeState.getPeers
      blockPeriod <- fromIntegral . CAP.blockPeriod . Block.getConsensus <$> getLastBlock
      if length peers <= 1
        then do
          Log.info "Not enough peers yet"
          liftIO (threadDelay blockPeriod) >> joinNetwork
        else do
          -- Join the network by asking for next block
          lastBlockIdx <- Block.index <$> NodeState.getLastBlock
          getBlockAtIdx1Msg <- Msg.mkGetBlockAtIdxMsg $ lastBlockIdx + 1
          nsendPeersSigned Msg.Messaging getBlockAtIdx1Msg

          addr <- NodeState.askSelfAddress
          accE <- NodeState.lookupAccount addr
          case accE of
            Left err -> do
              submitNewNodeAccountTx
              Log.info "My account is not in the ledger yet"
              liftIO (threadDelay blockPeriod) >> joinNetwork
            Right acc -> pure ()

    submitNewNodeAccountTx :: NodeT m ()
    submitNewNodeAccountTx = do
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
          Cmd.nsendTransaction Msg.Messaging newAccTx
