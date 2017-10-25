{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module NodeState
  ( NodeAccType(..)
  , NodeConfig(..)
  , NodeState(..)
  , NodeReaderT
  , NodeStateT
  , NodeT
  , runNodeT
  , spawnLocalT
  , NodeProcessT
  , initNodeState

  -- ** Node Peers
  , Peers
  , Peer(..)
  , peersToPids
  , peersToNodeIds
  , peersToAddresses
  , nodeIdToHostname

  -- ** Using NodeState DBs
  , withAcctDB
  , withAssetDB
  , withContractDB
  , withBlockDB

  -- ** Getters & Setters
  , askConfig
  , askDBPath

  , askAccount
  , askPrivateKey
  , askAccountType
  , askSelfAddress
  , askGenesisBlock

  -- ** World State
  , getLedger
  , setLedger

  -- ** Node Peers
  , getPeers
  , getPeerNodeIds
  , setPeers
  , withPeers
  , modifyPeers
  , modifyPeers_

  -- ** Memory Pool
  , appendTxMemPool
  , getTxMemPool
  , resetTxMemPool
  , pruneTxMemPool
  , removeTxsFromMemPool
  , isTxUnique
  , isTestNode

  -- ** Query Ledger State
  , lookupAccount
  , withAccount
  , withLedgerState

  -- ** World State
  , applyBlock
  , syncNodeStateWithDBs

  -- ** Consensus
  , getPoAState
  , setPoAState
  , modifyPoAState_
  , getLatestBlock
  , setLatestBlock
  , isValidatingNode
  , getValidatorPeers

  -- ** Peer persistence
  , loadPeers
  , savePeers

  , withApplyCtx

  ) where

import Protolude

import Control.Monad.Base

import qualified Control.Concurrent.MVar as MVar
import Control.Distributed.Process as DP
import qualified Control.Distributed.Process.Node as DPN

import Data.Aeson (ToJSON(..), object, (.=))
import Data.List ((\\))
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.DList as DL
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Serialize as Serialize

import Network.Transport

import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.Posix.Files (ownerReadMode, ownerWriteMode, setFileMode, unionFileModes)

import Address (Address)
import qualified Account
import qualified Address
import qualified Block
import qualified Config
import qualified Key
import qualified DB
import qualified Ledger
import qualified Transaction
import qualified TxLog
import qualified MemPool
import qualified Hash
import qualified Validate
import qualified Consensus.Authority.Types as CAT

data Peer = Peer
  { peerPid     :: ProcessId  -- ProcessId of Peer Controller Process
  , peerAccAddr :: Address    -- Address of Peer node account
  } deriving (Eq, Ord, Show, Generic, Binary.Binary)

type Peers = Set.Set Peer

peersToPids :: Peers -> [ProcessId]
peersToPids = map peerPid . Set.toList

peersToNodeIds :: Peers -> [NodeId]
peersToNodeIds = map (processNodeId . peerPid) . Set.toList

peersToAddresses :: Peers -> [Address]
peersToAddresses = map peerAccAddr . Set.toList

pidToNodeIdBS :: ProcessId -> ByteString
pidToNodeIdBS = endPointAddressToByteString . nodeAddress . processNodeId

nodeIdToHostname :: NodeId -> ByteString
nodeIdToHostname = toS . takeWhile (/= ':') . toS . endPointAddressToByteString . nodeAddress

data NodeState = NodeState
  { ledger      :: MVar Ledger.World    -- ^ In-memory world-state
  , p2pPeers    :: MVar Peers           -- ^ Known peers in p2p network
  , txPool      :: MVar MemPool.MemPool -- ^ Transactions memory pool
  , poaState    :: MVar CAT.PoAState     -- ^ Stateful values related to consensus
  , latestBlock :: MVar Block.Block     -- ^ Latest block in the chain
  }

data NodeAccType = New | Existing

data NodeConfig = NodeConfig
  { account      :: Account.Account    -- ^ Active account
  , nodePrivKey  :: Key.PrivateKey     -- ^ Active account's private key
  , accountType  :: NodeAccType        -- ^ Is account new or existing
  , genesisBlock :: Block.Block        -- ^ Genesis block
  , config       :: Config.Config      -- ^ Node configuration
  , databases    :: DB.Databases       -- ^ Record of DBs node has open
  }

initNodeState
  :: MonadIO m
  => Ledger.World
  -> Peers
  -> MemPool.MemPool
  -> CAT.PoAState
  -> Block.Block
  -> m NodeState
initNodeState w ps mp poa blk = do
  ledger <- liftIO (newMVar w)
  p2pPeers <- liftIO (newMVar ps)
  txPool <- liftIO (newMVar mp)
  poaState <- liftIO (newMVar poa)
  latestBlock <- liftIO (newMVar blk)
  return NodeState{..}

type NodeReaderT = ReaderT NodeConfig
type NodeStateT  = StateT NodeState
type NodeT m     = NodeStateT (NodeReaderT m)

-- | Run a computation with access to NodeConfig environment and NodeState
-- state, with any base monad as long as MonadIO is implemented
runNodeT :: MonadIO m => NodeConfig -> NodeState -> NodeT m a -> m a
runNodeT nodeConfig nodeState = flip runReaderT nodeConfig . flip evalStateT nodeState

type NodeProcessT a = NodeT Process a

-- | Spawn/run a NodeProcessT process
spawnLocalT :: NodeProcessT () -> NodeProcessT ProcessId
spawnLocalT nproc = do
  nodeConfig <- lift ask
  nodeState <- get
  liftBase $ spawnLocal $
    runNodeT nodeConfig nodeState nproc

-- | Necessary Orphan for using `liftBase` in NodeProcessT code
instance MonadBase Process Process where
  liftBase = identity

-------------------------------------------------------------------------------
-- DB usage
-------------------------------------------------------------------------------

withDB
  :: MonadIO m
  => (DB.Databases -> db)
  -> (db -> NodeT m a)
  -> NodeT m a
withDB getDB f = f =<< fmap (getDB . databases) ask

withAcctDB :: MonadIO m => (DB.AcctDB -> NodeT m a) -> NodeT m a
withAcctDB = withDB DB.accountDB

withAssetDB :: MonadIO m => (DB.AssetDB -> NodeT m a) -> NodeT m a
withAssetDB = withDB DB.assetDB

withContractDB :: MonadIO m => (DB.ContractDB -> NodeT m a) -> NodeT m a
withContractDB = withDB DB.contractDB

withBlockDB :: MonadIO m => (DB.BlockDB -> NodeT m a) -> NodeT m a
withBlockDB  = withDB DB.blockDB

-------------------------------------------------------------------------------
-- Getters & Setters
-------------------------------------------------------------------------------

readMVar' :: MonadIO m => (a -> b) -> MVar a -> m b
readMVar' f = liftIO . fmap f . readMVar

modifyNodeState_
  :: MonadIO m
  => (NodeState -> MVar a)
  -> (a -> IO a)
  -> NodeT m ()
modifyNodeState_ g f = do
  mvar <- gets g
  liftIO $ modifyMVar_ mvar f

modifyNodeState
  :: MonadIO m
  => (NodeState -> MVar a)
  -> (a -> IO (a,b))
  -> NodeT m b
modifyNodeState g f = do
  mvar <- gets g
  liftIO $ modifyMVar mvar f

-------------------------------------------------------------------------------
-- Getters & Setters
-------------------------------------------------------------------------------

askConfig :: Monad m => NodeReaderT m Config.Config
askConfig = asks config

askDBPath :: Monad m => NodeReaderT m FilePath
askDBPath = Config.dbpath <$> askConfig

askAccount :: Monad m => NodeReaderT m Account.Account
askAccount = asks account

askPrivateKey :: Monad m => NodeReaderT m Key.PrivateKey
askPrivateKey = asks nodePrivKey

askAccountType :: Monad m => NodeReaderT m NodeAccType
askAccountType = asks accountType

askSelfAddress :: Monad m => NodeReaderT m Address.Address
askSelfAddress = Account.address <$> askAccount

-- | Returns the genesis block, used when joining a network
askGenesisBlock :: Monad m => NodeReaderT m Block.Block
askGenesisBlock = genesisBlock <$> ask

getLedger :: MonadIO m => NodeT m Ledger.World
getLedger = liftIO . readMVar =<< gets ledger

setLedger :: MonadIO m => Ledger.World -> NodeT m ()
setLedger ledger' = modifyNodeState_ ledger $ const $ pure ledger'

withLedgerState :: MonadIO m => (Ledger.World -> NodeT m a) -> NodeT m a
withLedgerState f = f =<< liftIO . readMVar =<< fmap ledger get

withPeers :: MonadIO m => (Peers -> NodeT m a) -> NodeT m a
withPeers f = f =<< getPeers

getPeers :: MonadIO m => NodeT m Peers
getPeers = liftIO . readMVar =<< gets p2pPeers

getPeerNodeIds :: MonadIO m => NodeT m [NodeId]
getPeerNodeIds = peersToNodeIds <$> getPeers

getPoAState :: MonadIO m => NodeT m CAT.PoAState
getPoAState = liftIO . readMVar =<< gets poaState

setPoAState :: MonadIO m => CAT.PoAState -> NodeT m ()
setPoAState pstate =
  modifyNodeState_ poaState $ const $ pure pstate

modifyPoAState_ :: MonadIO m => (CAT.PoAState -> CAT.PoAState) -> NodeT m ()
modifyPoAState_ f = modifyNodeState_ poaState $ pure . f

getLatestBlock :: MonadIO m => NodeT m Block.Block
getLatestBlock = liftIO . readMVar =<< gets latestBlock

setLatestBlock :: MonadIO m => Block.Block -> NodeT m ()
setLatestBlock = modifyNodeState_ latestBlock . const . pure

setPeers :: MonadIO m => Peers -> NodeT m ()
setPeers = modifyNodeState_ p2pPeers . const .  pure

-- | Modify peers atomically
modifyPeers_ :: MonadIO m => (Peers -> Peers) -> NodeT m ()
modifyPeers_ f = modifyNodeState_ p2pPeers $ pure . f

-- | Modify peers atomically, returning a result
modifyPeers :: MonadIO m => (Peers -> (Peers,a)) -> NodeT m a
modifyPeers f = modifyNodeState p2pPeers $ pure . f

-- | Insert transaction into transaction pool
appendTxMemPool :: MonadIO m => Transaction.Transaction -> NodeT m ()
appendTxMemPool tx = modifyNodeState_ txPool $ pure . MemPool.appendTx tx

getTxMemPool :: MonadIO m => NodeT m MemPool.MemPool
getTxMemPool = liftIO . readMVar =<< gets txPool

resetTxMemPool :: MonadIO m => NodeT m ()
resetTxMemPool = modifyNodeState_ txPool $ const $ pure MemPool.emptyMemPool

-- | Atomically remove all invalid transactions from the mempool
-- and return the valid transactions.
pruneTxMemPool :: MonadIO m => NodeT m ([Transaction.Transaction],[Transaction.InvalidTransaction])
pruneTxMemPool =
  withLedgerState $ \world -> do
    nodeConfig <- ask
    nodeState  <- get
    modifyNodeState txPool $ \memPool -> do
      let memPoolTxs = DL.toList $ MemPool.transactions $ memPool
      validTxs <- runNodeT nodeConfig nodeState $ withApplyCtx $ \applyCtx ->
        liftIO $ Validate.validateTransactions applyCtx world memPoolTxs
      case validTxs of
        Right _ -> pure (memPool, (memPoolTxs,[]))
        Left errs ->
          let invalidTxs   = flip map errs $ \(Transaction.InvalidTransaction tx _) -> tx
              newMemPool   = MemPool.removeTxs memPool invalidTxs
              txsInMemPool = DL.toList $ MemPool.transactions newMemPool
          in pure (newMemPool, (txsInMemPool, errs))

-- | Atomically remove all specified transactions from the MemPool
removeTxsFromMemPool :: MonadIO m => [Transaction.Transaction] -> NodeT m ()
removeTxsFromMemPool txs  =
  modifyNodeState_ txPool $ \memPool ->
    pure $ MemPool.removeTxs memPool txs

isTxUnique :: Transaction.Transaction -> MonadIO m => NodeT m Bool
isTxUnique tx = pure . flip MemPool.isTxUnique tx =<< getTxMemPool

isTestNode :: Monad m => NodeReaderT m Bool
isTestNode = Config.testMode <$> askConfig

isValidatingNode :: MonadIO m => NodeT m Bool
isValidatingNode = do
  validatorAddrs <- CAT.unValidatorSet <$> getValidatorSet
  selfAddr <- lift askSelfAddress
  return $ selfAddr `Set.member` validatorAddrs

getValidatorSet :: MonadIO m => NodeT m CAT.ValidatorSet
getValidatorSet = do
  latestBlock <- getLatestBlock
  let poa = Block.consensus $ Block.header latestBlock
  return $ CAT.validatorSet poa

-- | Returns list of peers that are validating nodes
getValidatorPeers :: MonadIO m => NodeT m Peers
getValidatorPeers = do
  peers <- getPeers
  validatorAddrs <- CAT.unValidatorSet <$> getValidatorSet
  return $ flip Set.filter peers $ \peer ->
    peerAccAddr peer `Set.member` validatorAddrs

withApplyCtx :: MonadIO m => (Validate.ApplyCtx -> NodeT m a) -> NodeT m a
withApplyCtx f = do
  latestBlk   <- getLatestBlock
  nodeAddress <- lift askSelfAddress
  nodePrivKey <- lift askPrivateKey
  f Validate.ApplyCtx
    { applyCurrBlock   = latestBlk
    , applyNodeAddress = nodeAddress
    , applyNodePrivKey = nodePrivKey
    }

-------------------------------------------------------------------------------
-- Query Ledger (World) state
-------------------------------------------------------------------------------

lookupInLedger :: MonadIO m => (Ledger.World -> a) -> NodeT m a
lookupInLedger f = withLedgerState $ return . f

lookupAccount
  :: MonadIO m
  => Address.Address
  -> NodeT m (Either Ledger.AccountError Account.Account)
lookupAccount = lookupInLedger . Ledger.lookupAccount

withAccount
  :: MonadIO m
  => Address.Address
  -> (Account.Account -> NodeT m a)
  -> NodeT m (Either Ledger.AccountError a)
withAccount addr f = do
  eAcc <- lookupAccount addr
  case eAcc of
    Left err -> pure $ Left err
    Right acc -> Right <$> f acc

-------------------------------------------------------------------------------
-- Sync Ledger State & DB
-------------------------------------------------------------------------------

applyBlock
  :: MonadIO m
  => Block.Block
  -> NodeT m (Either Transaction.InvalidTransaction ())
applyBlock block =
  withLedgerState $ \ledgerState ->
    withApplyCtx $ \applyCtx -> do
      (newWorld, errs, deltasMap) <- liftIO $
        Validate.applyBlock applyCtx ledgerState block
      -- New block should only be applied if 0 errors in block
      case head errs of
        Just err -> return $ Left err
        Nothing  -> fmap Right $ do
          -- Atomically remove transactions in this block from NodeState mempool
          removeTxsFromMemPool $ Block.transactions block

          -- Update Latest block in NodeState
          setLatestBlock block

          -- Update New World state in NodeState
          setLedger newWorld

          -- Write Deltas collected during applyBlock to TxLog
          let blockIdx = Block.index block
          dbPath <- lift askDBPath

          -- Write the entire block's transacction list to TxLog in database
          liftIO $ forM_ (Map.toList deltasMap) $ do
            uncurry $ TxLog.writeDeltas (fromIntegral blockIdx) (DB.txLogFile dbPath)
            uncurry $ TxLog.writeDeltasJSON (fromIntegral blockIdx) (DB.txLogFile dbPath)

syncNodeStateWithDBs :: MonadIO m => NodeT m ()
syncNodeStateWithDBs = syncWorldWithDBs >> syncLatestBlockWithDBs

syncWorldWithDBs :: MonadIO m => NodeT m ()
syncWorldWithDBs = do
  dbs <- databases <$> ask
  withLedgerState $ liftIO . DB.syncWorld dbs

-- | Since blocks are not stored in ledger(world) state, we
-- must sync them to the DB separately
syncLatestBlockWithDBs :: MonadIO m => NodeT m ()
syncLatestBlockWithDBs = do
  latestBlock' <- getLatestBlock
  withBlockDB $ liftIO . flip DB.writeBlock latestBlock'

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

instance ToJSON Peer where
  toJSON (Peer pid addr) = object
    [ "tag" .= ("Peer" :: Text)
    , "contents" .= object
        [ "peerPid" .= decodeUtf8 (pidToNodeIdBS pid)
        , "peerAccAddr" .= addr
        ]
    ]

-------------------------------------------------------------------------------
-- Peer Persistence
-------------------------------------------------------------------------------

peersFile :: FilePath -> FilePath
peersFile root = root </> "peers"

loadPeers :: FilePath -> IO Peers
loadPeers root = do
   let peersFile' = peersFile root
   peersFileExists <- doesFileExist peersFile'
   if peersFileExists then
     Binary.decode <$> BSL.readFile peersFile'
   else pure Set.empty

savePeers :: FilePath -> Peers -> IO ()
savePeers root peers = do
   let peersFile' = peersFile root
   BSL.writeFile peersFile' $ Binary.encode peers
   let mode = unionFileModes ownerReadMode ownerWriteMode
   setFileMode peersFile' mode
