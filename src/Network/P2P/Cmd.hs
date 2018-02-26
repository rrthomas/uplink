{-|

Intraprocess protocol used to mediate communication between P2P and RPC
interfaces.

--}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.P2P.Cmd (
  Cmd(..),
  CmdResult(..),

  tasksProc,

  TestCmd(..),
  handleTestCmd,

  commTasksProc,
  commTasksProc',

  newTransaction,
  nsendTransaction,
) where

import Protolude hiding (put, get, newChan)

import Control.Monad (fail)
import Control.Monad.Base

import Control.Distributed.Process.Lifted
import Control.Distributed.Process.Lifted.Class

import qualified Data.ByteString as BS
import qualified Data.Map as Map

import Data.Binary
import qualified Data.Text as Text
import qualified Data.Serialize as S

import Node.Peer
import NodeState
import SafeString
import qualified Account
import qualified Address
import qualified Asset
import qualified Config
import qualified DB
import qualified Ledger
import qualified Contract
import qualified Key
import qualified Storage
import qualified Utils
import qualified Time
import qualified Transaction
import qualified Derivation
import Network.Utils
import Network.P2P.Controller
import Network.P2P.Service (Service(..))
import qualified Network.P2P.Logging as Log
import qualified Network.P2P.Message as Message

import Script.Eval as Eval
import Script.Compile as Compile

import System.Random

-------------------------------------------------------------------------------
-- P2P Commands
-------------------------------------------------------------------------------

data Cmd
  = Test TestCmd      -- ^ Test commands
  | Transaction Transaction.Transaction -- ^ Transaction
  | ListAccounts
  | ListAssets
  | ListContracts
  | Discover
  | Reconnect
  | ListPeers
  | AddPeer Text
  | Ping
  | PingPeer Text
  deriving (Eq, Show, Generic, Binary)

-- | A datatype defining Cmds that should only be able to be
-- executed when the node is booted in a test state with '--test'
data TestCmd

  -- Cmd to saturate the network with 'nTxs' transactions spaced 'nSecs' apart
  = SaturateNetwork
    { nTxs :: Int   -- ^ Number of transactions to be created
    , nSecs :: Int  -- ^ Over this many seconds
    }

  -- ^ Cmd to restart a process after a given delay
  | ServiceRestartController
    { ctrlService :: Service -- ^ Service to restart
    , ctrlDelay   :: Int     -- ^ Time to delay restart in microseconds
    }

  -- ^ Cmd to reset mempool of entire network.
  | ResetMemPools

  -- ^ Cmd to reset node DB
  | ResetDB
    { address   :: Address.Address -- ^ Address of pubkey used to sign this tx
    , signature :: ByteString      -- ^ Signature of address to prove possesion of priv key
    }
  deriving (Eq, Show, Generic, Binary)

data CmdResult
  = CmdResult Text
  | Accounts [Account.Account]
  | Assets [Asset.Asset]
  | Contracts [Contract.Contract]
  | PeerList Peers
  | CmdFail Text
  deriving (Show, Generic, Binary)-- XXX

cmdSuccess :: CmdResult
cmdSuccess = CmdResult "Success"

-------------------------------------------------------------------------------
-- P2P Tasks Proc
-------------------------------------------------------------------------------

-- | Process that receives commands from external processes like the RPC server
-- and/or the Console process. These commands get evaluated and then a CmdResult
-- is returned
tasksProc
  :: forall m. (MonadProcessBase m, DB.MonadReadWriteDB m)
  => Service
  -> NodeT m ()
tasksProc service = do
    controlP $ \runInBase ->
      forever $ runInBase $
        onConsoleMsg =<< expect
  where
    onConsoleMsg :: (Cmd, SendPort CmdResult) -> NodeT m ()
    onConsoleMsg (cmd, sp) = do
      Log.info $ "Recieved Cmd:\n\t" <> show cmd
      res <- handleCmd service cmd
      sendChan sp res


-- | Issue a Cmd to the "tasks" process and wait for a CmdResult
-- Note: This function is blocking and will wait forever for a response. Use
-- `commCmdProc'` to specify a timeout for how long to wait for a response.
commTasksProc :: MonadProcessBase m => Cmd -> m CmdResult
commTasksProc cmd = do
  (sp,rp) <- newChan
  nsend (show Tasks) (cmd, sp)
  receiveChan rp

-- | Issue a Cmd to the "tasks" process and wait for a CmdResult
-- Note: This function is blocking and will wait `timeout` ms for a response.
commTasksProc' :: MonadProcessBase m => Int -> Cmd -> m (Maybe CmdResult)
commTasksProc' timeout cmd = do
  (sp,rp) <- newChan
  nsend (show Tasks) (cmd, sp)
  receiveChanTimeout timeout rp

-------------------------------------------------------------------------------
-- P2P Command handlers
-------------------------------------------------------------------------------

-- | This function handles Cmd messages originating from either the shell
-- or the RPC interface which translates RPCCmds into Cmds
handleCmd
  :: (MonadProcessBase m, DB.MonadReadWriteDB m)
  => Service
  -> Cmd
  -> NodeT m CmdResult
handleCmd service cmd =
  case cmd of
    ListAccounts -> do
      world <- NodeState.getLedger
      return $ Accounts $ Map.elems $ Ledger.accounts world
    ListAssets -> do
      world <- NodeState.getLedger
      return $ Assets $ Map.elems $ Ledger.assets world
    ListContracts -> do
      world <- NodeState.getLedger
      return $ Contracts $ Map.elems $ Ledger.contracts world
    Transaction tx -> do
      nsendTransaction service tx
      return cmdSuccess
    Test testCmd   -> do
      testNode <- NodeState.isTestNode
      if testNode
        then handleTestCmd testCmd
        else Log.warning "Node is not in test mode. Command ignored."
      return cmdSuccess
    Discover -> do
      peers <-  getPeerNodeIds
      liftP $ mapM_ doDiscover peers
      return cmdSuccess
    Reconnect -> do
      pid <- getSelfPid
      reconnect pid
      return cmdSuccess
    Ping -> do
      peers <- queryAllPeers
      nodeId <- liftP extractNodeId
      forM_ peers $ \peer -> do
        let msg = SafeString.fromBytes' (toS nodeId)
        nsendPeer' service peer $ Message.Ping msg
      return cmdSuccess
    (PingPeer host) -> do
      nodeId <- extractNodeId
      let msg = SafeString.fromBytes' (toS nodeId)
      eNodeId <- liftIO $ mkNodeId (toS host)
      case eNodeId of
        Left err     -> Log.warning err
        Right nodeId -> nsendPeer' service nodeId $ Message.Ping msg

      return cmdSuccess
    ListPeers -> do
      peers <- NodeState.getPeers
      return $ PeerList peers
    (AddPeer host) -> do
      eNodeId <- liftIO $ mkNodeId (toS host)
      case eNodeId of
        Left err     -> do
          Log.warning err
          return $ CmdFail err
        Right nodeId -> do
          liftP $ doDiscover nodeId
          return cmdSuccess


handleTestCmd
  :: (MonadProcessBase m, DB.MonadReadWriteDB m)
  => TestCmd
  -> NodeT m ()
handleTestCmd testCmd =
  case testCmd of

    SaturateNetwork ntxs nsecs -> do
      -- if 0 seconds, submit txs with no delay
      let tps = fromIntegral ntxs / fromIntegral (max 1 nsecs)
      let delay = if (nsecs == 0 || tps == 0) then 0 else 1 / tps
      txs <- replicateM ntxs $ do
          (newAcc, acctKeys) <- liftIO $ Account.newAccount "GMT" mempty
          let tz = Account.timezone newAcc
              md = Account.metadata newAcc
              pub = Key.unHexPub $ Key.hexPub $ Account.publicKey newAcc
          let txHdr = Transaction.TxAccount $ Transaction.CreateAccount pub tz md

          liftIO $ Transaction.newTransaction (Account.address newAcc) (snd acctKeys) txHdr

      -- Submit txs
      case delay of
        0 -> nsendTransactionMany TestMessaging txs
        _ -> Utils.delayedReplicateM_ ntxs delay $
              void $ spawnLocal $
                forM_ txs $ nsendTransaction TestMessaging

    -- XXX Handle error cases better
    ServiceRestartController service delay -> do
      peers <- queryCapablePeers service
      randN <- liftIO $ randomRIO (0, length peers)
      case peers `atMay` randN of
        Nothing -> return ()
        Just nodeId -> do
          let serviceNmSafe = SafeString.fromBytes $ show service
          let msg = Message.ServiceRestart $ Message.ServiceRestartMsg service delay
          nsendPeer' service nodeId (Message.Test msg)

    ResetMemPools -> do
      let resetMsg = Message.ResetMemPool Message.ResetMemPoolMsg
      nsendPeers' TestMessaging (Message.Test resetMsg)

    ResetDB addr sig' -> do
      case Key.decodeSig sig' of
        Left err -> Log.warning $ show err
        Right sig -> do

          let addrBS = Address.rawAddr addr
          backend  <- Config.storageBackend <$> NodeState.askConfig
          nodeAcc  <- NodeState.askAccount
          nodeKeys <- NodeState.askKeyPair

          if not (Key.verify (fst nodeKeys) sig addrBS)
            then do
              let errMsg = Text.intercalate "\n    "
                    [ "Error resetting DB:"
                    , "Could not verify the signature of the address sent in ResetDB TestCmd."
                    , "Please sign the address associated with the uplink node account using the uplinks node's private key"
                    ]
              Log.warning errMsg
            else do
              -- Wipe DB entirely, keeping Node Keys and Node Acc
              eRes <- lift $ first show <$> DB.resetDB
              case eRes of
                Left err -> Log.critical $
                  "Failed to reset Databases on ResetDB TestCmd: " <> err
                Right _ ->
                  -- Wipe entire node state except for peers (fresh world contains
                  -- preallocated accounts specified in config)
                  NodeState.resetNodeState

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

nsendTransaction
  :: MonadProcessBase m
  => Service
  -> Transaction.Transaction
  -> m ()
nsendTransaction service tx = do
  let message = Message.SendTx (Message.SendTransactionMsg tx)
  -- Don't need to `nsendCapable` because incapable
  -- peers simply won't process the transaction
  nsendPeers' service message

nsendTransactionMany
  :: MonadProcessBase m
  => Service
  -> [Transaction.Transaction]
  -> m ()
nsendTransactionMany service txs = do
  let messages = [Message.SendTx (Message.SendTransactionMsg tx) | tx <- txs]
  -- Don't need to `nsendCapable` because incapable
  -- peers simply won't process the transaction
  nsendPeersMany' service messages

-- | Creates new transaction using current node account and private key
newTransaction
  :: MonadIO m
  => Transaction.TransactionHeader
  -> NodeT m Transaction.Transaction
newTransaction txHeader = do
  privKey <- askPrivateKey
  accAddr <- askSelfAddress
  liftIO $ Transaction.newTransaction accAddr privKey txHeader

getTransaction :: Transaction.Transaction -> Process ()
getTransaction tx = putText "Writing transaction"
