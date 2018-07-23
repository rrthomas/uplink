{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TestNetwork (
  networkTests
) where

import Test.Tasty
import qualified Test.QuickCheck.Monadic as QC
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Control.Concurrent.MVar
import Control.Distributed.Process.Node (LocalNode, runProcess, localNodeId, initRemoteTable, closeLocalNode)
import qualified Control.Distributed.Process as DP
import Control.Exception.Lifted (bracket_)
import Control.Monad.Catch (bracket)
import System.Directory
import Control.Monad (void, replicateM, replicateM_, when, forever, fail)
import Data.Foldable
import System.Random (randomRIO)
import Data.Monoid
import Control.Concurrent (threadDelay)
import qualified Database.LevelDB.Base as LevelDB
import Data.Time.Clock
import Data.List (partition)

import Database.PostgreSQL.Simple
import qualified Database.PostgreSQL.Tmp as PGTmp


import Protolude hiding (head, tail, bracket)
import Prelude (zip3, tail, head, unlines)
import Config (Config(..), Transport(TCP), handleConfig, defaultConfig)
import Ledger (World)
import Address (deriveAddress, Address(..), AAccount)
import Account(AccountKeyOpt(..))
import qualified Utils
import qualified Network.P2P as P2P
import qualified Network.P2P.Service as S
import qualified Network.P2P.Logging as Log
import qualified Network.P2P.Logging.Rule as Log
import qualified Network.P2P.Cmd as Cmd
import qualified Network.P2P.Controller as Ctrl
import qualified Network.Transport as NT
import qualified Key
import Opts
import Driver (driverChain)
import Transaction
import Transaction.RandomGen
import Reference
import Hash
import qualified Encoding as E
import qualified DB
import qualified DB.LevelDB

testDir :: FilePath
testDir = "test-uplink-net"

testConfigFile :: FilePath
testConfigFile = "config/node.config.local"

testNodePath :: FilePath
testNodePath = testDir <> "/" <> "uplink-test-node"

nodePortBase :: Int
nodePortBase = 8000

oneSecond :: Int
oneSecond = 1000000

validatorsPath :: FilePath
validatorsPath = "config/validators/auth"

data TestDB
  = TestLevelDB
  | TestPostgres
  deriving (Show, Eq)

data TestError
  = ExpectTimeoutError
  | InvalidBlockHashes
  | InvalidTransactions
  deriving Show

data NetworkParameters
  = NetworkParameters
    { nNodes :: Int
    , nTxs :: Int
    , nValidators :: Int
    } deriving Show


testNetwork
  :: TestDB
  -> NetworkParameters
  -> Assertion
testNetwork db np = do

    putText $ Utils.ppShow $ fmap (toS :: [Char] -> Text)
      [ "#Nodes: " <> show (nNodes np)
      , "#Transactions: " <> show (nTxs np)
      , "#Validators: " <> show (nValidators np)
      ]

    testConfig <- handleConfig False testConfigFile

    -- Create N pub/priv keys
    keys <- replicateM (nNodes np) Key.new
    let privKeys = snd <$> keys
        addrs = (deriveAddress . fst <$> keys) :: [Address AAccount]

    rndTime <- diffTimeToPicoseconds . utctDayTime <$> getCurrentTime
    let configs = fmap (mkConfig testConfig rndTime) (zip ports paths)

    -- Create and tear down testing directories
    -- Currently, we are leaving the directory intact (not deleting it)
    -- for debugging purposes after each test
    createTestingDir

    -- Create and run master and children processes
    success <- masterNode configs privKeys addrs

    -- Cleanup Postgres
    removeTmpDatabase rndTime ports

    case success of
      Left err ->
        assertBool (show err :: [Char]) False
      Right _ ->
        assertBool "" True

  where
    masterNode :: [Config] -> [Key.PrivateKey] -> [Address AAccount] -> IO (Either TestError ())
    masterNode configs privKeys addrs =
      bracket
        (P2P.createLocalNodeWithTransport' TCP "127.0.1.1" (show nodePortBase) (Just initRemoteTable))
        (fmap Right . cleanMasterNode)
        $ \(masterNode, transport) -> do
            -- Create local cloud haskell node
            successMVar <- newEmptyMVar

            let (validatorConfigs, nonValidatorConfigs') = splitAt (nValidators np) configs
            let (validatorPKs, nonValidatorPKs) = splitAt (nValidators np) privKeys

            runProcess masterNode $

              -- Spawn the validators
              bracket
                (spawnUplinkNodes (zip validatorConfigs validatorPKs) True)
                cleanUplinkNodes
                $ \validators -> do
                  liftIO $ threadDelay oneSecond

                  -- Make validator discover themselves
                  let validatorIds = map (localNodeId . fst) validators
                  forM_ validatorIds $ \nid -> do
                    DP.say $ "Sending DiscoverPeers message to " <> show nid
                    commTasksProcRemote nid (Cmd.DiscoverPeers validatorIds)
                    -- Need to wait ~1 second such that peer discovery doesn't deadlock
                    liftIO $ threadDelay oneSecond

                  let validatorAddresses = NT.endPointAddressToByteString . DP.nodeAddress <$> validatorIds
                  -- Add validators nodes to bootnodes config file to notify others
                  let nonValidatorConfigs = addBootnodes validatorAddresses <$> nonValidatorConfigs'

                  -- Spawn nonValidators uplink nodes with validators set as bootnodes
                  bracket
                    (spawnUplinkNodes (zip nonValidatorConfigs nonValidatorPKs) False)
                    cleanUplinkNodes
                    $ \nonValidators -> do
                        let nonValidatorIds = map (localNodeId . fst) nonValidators

                        -- Generate and send a bunch of transactions
                        let uplinkNodeIds = validatorIds ++ nonValidatorIds
                        txs <- sendRndTransactions (nTxs np) uplinkNodeIds

                        if length txs == (nTxs np)
                          then do
                             -- check if ledger is in consistent state
                             success <- checkSuccess uplinkNodeIds 0
                             liftIO $ putMVar successMVar success
                          else liftIO $ putMVar successMVar (Left InvalidTransactions)

            -- Give some time to the transport layer to close properly
            -- before starting another test
            threadDelay (5 * oneSecond)
            takeMVar successMVar

    mkConfig cfg rndTime (port, path) =
      cfg { port = port
          , nodeDataDir = path
          , testMode = True
          -- This rndTime hack is done because levelDB complains if
          -- we create a database with the same path when running it with QuickCheck
          -- even though we remove the path after each iteration
          , storageBackend = storageBackend rndTime port path
          , rpcPort = 0
          , bootnodes = []
          , loggingRules = [Log.LogRule
              { Log.ruleSource = Log.AnySource
              , Log.ruleSeverity = Log.AnySeverity
              , Log.ruleDestination = Log.LogFile (path ++ "-log") Log.Colors
              }]
          }

    cleanMasterNode (masterNode, transport) = closeLocalNode masterNode >> NT.closeTransport transport

    cleanUplinkNodes uplinkNodeData = forM_ uplinkNodeData $ \(uplinkNode, uplinkTransport) -> do
      liftIO (closeLocalNode uplinkNode)
      liftIO (NT.closeTransport uplinkTransport)

    storageBackend rndTime port path = case db of
      TestLevelDB -> DB.LevelDB (testDir <> "/" <> "uplinkdb" <> "/" <> show rndTime <> "/" <> path)
      TestPostgres -> DB.PostgreSQL $ testConnInfo rndTime port

    addBootnodes bootnodes cfg =
      cfg { bootnodes = bootnodes }
    ports = (nodePortBase +) <$> [1..(nNodes np)]
    paths = (testNodePath ++) . show <$> [1..(nNodes np)]

    spawnUplinkNodes :: [(Config, Key.PrivateKey)] -> Bool -> DP.Process [(LocalNode, NT.Transport)]
    spawnUplinkNodes nodesData isValidator =
      forM (zip nodesData [0..]) $ \((cfg, pk), idx) -> do

        nidAndTpMVar <- liftIO newEmptyMVar

        if isValidator
          then liftIO $ forkIO $
            driverChain (Init (Just (Existing (validatorsPath <> show idx <> "/key"))) Nothing) cfg (Just nidAndTpMVar)
          else liftIO $ forkIO $
            driverChain (Init (Just (InMemory pk)) Nothing) cfg (Just nidAndTpMVar)

        (node, transport) <- liftIO $ readMVar nidAndTpMVar
        let nid = localNodeId node

        -- Wait until p2p process has been spawned before continuing to spawn
        -- other uplink nodes.
        mRes <- S.findRemoteService nid Ctrl.PeerController
        case mRes of
          Nothing -> panic $ "Failed to find peer controller for " <> show nid
          Just _  -> do
            liftIO $ threadDelay oneSecond
            pure (node, transport)

    createTestingDir = do
      dirExists <- doesDirectoryExist testDir
      when dirExists $
       removeDirectoryRecursive testDir
      createDirectory testDir

    removeTmpDatabase rndTime ports = when (db == TestPostgres) $
      forM_ ports
        (\port -> do
          conn' <- connect defaultConnectInfo  {
              connectUser     = "uplink_test_user"
            , connectPassword = "uplink_test_user"
            , connectDatabase = "postgres"
          }
          execute conn' "DROP DATABASE postgres_?_?" (rndTime, port)
        )


commTasksProcRemote :: DP.NodeId -> Cmd.TestCmd -> DP.Process Cmd.CmdResult
commTasksProcRemote nid cmd = do
  (sp,rp) <- DP.newChan
  DP.nsendRemote nid (show Cmd.ExternalCmd) (Cmd.Test cmd, sp)
  mRes <- DP.receiveChanTimeout (200 * oneSecond) rp
  case mRes of
    Nothing -> panic "Failed to comm with Tasks proc"
    Just res -> pure res

sendRndTransactions :: Int -> [DP.NodeId] -> DP.Process [Cmd.CmdResult]
sendRndTransactions nTxs uplinkNodeIds = do
  txs <- replicateM nTxs $ do
    nId <- liftIO $ rndItem uplinkNodeIds
    liftIO $ threadDelay (oneSecond `div` 3)
    commTasksProcRemote nId Cmd.GenerateTransaction
  -- Show invalid transactions for debugging
  let (validTxs, invalidTxs) = partition (== Cmd.GeneratedTransaction (Right ())) txs
  liftIO $ putText (show invalidTxs)
  pure validTxs

-- | Given a list of node ids of test network uplink nodes, check that all nodes
--   have the following equivalent states:
--     1) Last block hash
--     2) Ledger state
--     3) ... (to be determined)
checkSuccess
  :: [DP.NodeId] -- ^ List of node ids of uplink test network nodes
  -> Int
  -> DP.Process (Either TestError ())
checkSuccess nIds accDelay = do
  -- TestData
  --   { lastBlockHash :: Hash.Hash Encoding.Base16ByteString
  --   , ledgerState   :: Ledger.World
  --   }
  testData <- mapM (`commTasksProcRemote` Cmd.QueryNodeState) nIds
  if allTheSame testData
    then do
      liftIO $ putText $ "Consensus took: " <> show (accDelay `div` 1000) <> " ms"
      pure $ Right ()
    else
      -- Timing out after 100 without reaching consensus
      if accDelay > 100 * oneSecond
        then pure $ Left InvalidBlockHashes
        else do
          -- Keep polling until success
          let delay = oneSecond `div` 2
          liftIO $ threadDelay delay
          liftIO $ putText "No consensus yet"
          checkSuccess nIds (accDelay + delay)
  where
    allTheSame xs = all (== head xs) (tail xs)


-- | Generate a number from a range
genRange :: Int -> Int -> QC.Gen Int
genRange min max = QC.suchThat (abs <$> QC.arbitrary) (\x -> x >= min && x <= max)


networkParameters :: [NetworkParameters]
networkParameters =
  [ NetworkParameters 1 5 1
  , NetworkParameters 5 50 2
  ]


networkTests :: TestTree
networkTests =
  testGroup "Test Uplink Network"
    [ testGroup "Postgres" $
        fmap (\np -> testCase (show np) (testNetwork TestPostgres np)) networkParameters
 --    , testGroup "LevelDB" $
 --        fmap (\np -> testCase (show np) (testNetwork TestLevelDB np)) networkParameters
    ]

-- PostgreSQL default Uplink connection for testing
testConnInfo :: Integer -> Int -> ConnectInfo
testConnInfo rndTime port = defaultConnectInfo
  { connectUser     = "uplink_test_user"
  , connectPassword = "uplink_test_user"
  , connectDatabase = "postgres_" <> show rndTime <> "_" <> show port
  }
