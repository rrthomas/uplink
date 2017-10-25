{-|

Intraprocess protocol used to mediate communication between P2P and RPC
interfaces.

--}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Network.P2P.Cmd (
  Cmd(..),
  CmdResult(..),
  handleCmd,
  
  TestCmd(..),
  handleTestCmd,

  newTransaction,
  nsendTransaction,
) where

import Protolude

import Control.Monad.Base
import Control.Distributed.Process

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Binary
import NodeState
import SafeString
import qualified Account
import qualified Address
import qualified Asset
import qualified DB
import qualified Ledger
import qualified Contract
import qualified Key
import qualified Storage
import qualified Utils
import qualified Time
import qualified Transaction
import qualified Derivation
import qualified Logging as Log
import Network.Utils
import Network.P2P.Controller
import Network.P2P.Service
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
  = SaturateNetwork
    { nTxs :: Int   -- ^ Number of transactions to be created
    , nSecs :: Int  -- ^ Over this many seconds
    }
  | ServiceRestartController
    { ctrlService :: Service -- ^ Service to restart
    , ctrlDelay   :: Int     -- ^ Time to delay restart in microseconds
    }
  | ResetMemPools   -- ^ Cmd to reset mempool of entire network.
  deriving (Eq, Show, Generic, Binary)

data CmdResult 
  = CmdResult Text 
  | Accounts [Account.Account] 
  | Assets [Asset.Asset] 
  | Contracts [Contract.Contract] 
  | PeerList NodeState.Peers
  deriving (Show, Generic, Binary)-- XXX

cmdSuccess :: CmdResult
cmdSuccess = CmdResult "Success"


-------------------------------------------------------------------------------
-- P2P Command handlers
-------------------------------------------------------------------------------

-- | This function handles Cmd messages originating from either the shell
-- or the RPC interface which translates RPCCmds into Cmds
handleCmd
  :: Service
  -> Cmd
  -> NodeProcessT CmdResult
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
      liftBase $ nsendTransaction service tx
      return cmdSuccess
    Test testCmd   -> do
      testNode <- lift NodeState.isTestNode
      if testNode
        then handleTestCmd testCmd
        else Log.warning "Node is not in test mode. Command ignored."
      return cmdSuccess
    Discover -> do
      peers <-  getPeerNodeIds
      liftBase $ mapM_ doDiscover peers
      return cmdSuccess
    Reconnect -> do
      pid <- liftBase getSelfPid
      liftBase $ reconnect pid
      return cmdSuccess
    Ping -> do
      peers <- liftBase queryAllPeers
      nodeId <- liftBase extractNodeId
      forM_ peers $ \peer -> do
        let msg = SafeString.fromBytes' (toS nodeId)
        liftBase $ nsendPeer' service peer $ Message.Ping msg
      return cmdSuccess
    (PingPeer host) -> do 
      nodeId <- liftBase extractNodeId
      let msg = SafeString.fromBytes' (toS nodeId)
      peer <- liftIO $ mkNodeId (toS host)
      liftBase $ nsendPeer' service peer $ Message.Ping msg
      
      return cmdSuccess
    ListPeers -> do
      peers <- NodeState.getPeers 
      return $ PeerList peers
    (AddPeer host) -> do
      peer <- liftIO $ mkNodeId (toS host)
      liftBase $ doDiscover peer
      return cmdSuccess


handleTestCmd
  :: TestCmd
  -> NodeProcessT ()
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

          liftIO $ Transaction.newTransaction (Account.address newAcc) Nothing (snd acctKeys) txHdr

      -- Submit txs
      case delay of
        0 -> liftBase $ nsendTransactionMany TestMessaging txs
        _ -> liftBase $
          Utils.delayedReplicateM_ ntxs delay $
            void $ spawnLocal $
              forM_ txs $ nsendTransaction TestMessaging

        -- XXX Handle error cases better
    ServiceRestartController service delay -> do
      peers <- liftBase $ queryCapablePeers service
      randN <- liftIO $ randomRIO (0, length peers)
      case peers `atMay` randN of
        Nothing -> return ()
        Just nodeId -> do
          let serviceNmSafe = SafeString.fromBytes $ show service
          let msg = Message.ServiceRestart $ Message.ServiceRestartMsg service delay
          liftBase $ nsendPeer' service nodeId (Message.Test msg)

    ResetMemPools -> do
      let resetMsg = Message.ResetMemPool Message.ResetMemPoolMsg
      liftBase $ nsendPeers' TestMessaging (Message.Test resetMsg)

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

nsendTransaction :: Service -> Transaction.Transaction -> Process ()
nsendTransaction service tx = do
  let message = Message.SendTx (Message.SendTransactionMsg tx)
  -- Don't need to `nsendCapable` because incapable
  -- peers simply won't process the transaction
  nsendPeersAsync' service message

nsendTransactionMany :: Service -> [Transaction.Transaction] -> Process ()
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
  privKey <- lift askPrivateKey
  accAddr <- lift askSelfAddress
  liftIO $ Transaction.newTransaction accAddr Nothing privKey txHeader

getTransaction :: Transaction.Transaction -> Process ()
getTransaction tx = putText "Writing transaction"



