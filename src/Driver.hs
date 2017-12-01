{-|

Driver for command line option handling.

-}

{-# LANGUAGE ScopedTypeVariables #-}

module Driver (
  Command(..),
  driver,
) where

import Protolude hiding (link, newChan)

import qualified Control.Concurrent.Chan as Chan

import Control.Monad.Base
import Control.Distributed.Process hiding (try)
import qualified Control.Distributed.Process.Node as DPN
import Control.Distributed.Process.Node (initRemoteTable, runProcess)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Aeson as A

import Opts
import Config (Config(..), ChainSettings(..), handleChain)
import qualified RPC
import qualified Block
import qualified Key
import qualified Repl
import qualified Utils
import qualified Ledger
import qualified Script
import qualified Commit
import qualified Account
import qualified Console
import qualified NodeState
import qualified Authority
import qualified MemPool
import qualified Contract
import qualified Storage
import qualified TxLog
import qualified Logging as Log
import qualified Consensus.Authority.Params as CAP
import qualified Consensus.Authority.State  as CAS
import qualified Script.Pretty as Pretty
import qualified Script.Analysis as Anal
import qualified Script.Compile as Compile
import qualified Script.Typecheck as Typecheck
import System.Directory
import qualified System.Exit

import qualified DB
import qualified DB.PostgreSQL
import qualified DB.LevelDB

import Crypto.Random.EntropyPool as Entropy
import qualified Pedersen

import qualified Network.Transport.TCP as TCP

import Network.Utils
import qualified Network.P2P.Cmd as Cmd
import qualified Network.P2P as P2P
import qualified Network.P2P.Service as Service

import qualified XML
import qualified Text.XML.Expat.Tree as XML
import qualified Text.XML.Expat.Pickle as XML
import qualified Text.XML.Expat.Format as XML

--------------------------------------------------------------------------------
-- DB Agnostic State intialization
-- XXX Move to DB modules or typeclass XXX
--------------------------------------------------------------------------------

-- | Load the initial world, backend agnostic
loadWorld
  :: DB.MonadReadDB m
  => (m (Either Text Ledger.World) -> IO (Either Text Ledger.World))
  -> IO Ledger.World
loadWorld runDB = do
  eInitWorld <- runDB DB.readWorld
  case eInitWorld of
    Left err -> do
      Utils.putRed $ "Could not read world state from Database: " <> show err
      System.Exit.die "Database corrupt."
    Right initWorld' -> pure initWorld'

-- | Initialize world by loading preallocated accounts and writing them to DB
-- Note: Currently, using PoA, preallocated accounts are validator accounts
initWorld
  :: DB.MonadWriteDB m
  => Config
  -> (m (Either Text ()) -> IO (Either Text ()))
  -> IO ()
initWorld config runDB = do
  let preallocAccsDir = Config.preallocated config
  ePreallocatedAccs <- NodeState.loadPreallocatedAccs' preallocAccsDir
  case ePreallocatedAccs of
    Left err   -> do
      Utils.putRed $ "Failed to load preallocated accounts from directory: " <> show err
      System.Exit.die "Please run `uplink keys` to generate validator accounts."
    Right accs -> do
      let eInitWorld = Ledger.addAccounts accs mempty
      case eInitWorld of
        Left err -> do
          Utils.putRed $ "Failed to add validator accounts to world state: " <> show err
          System.Exit.die ""
        Right initWorld -> do
          eRes <- runDB $ DB.syncWorld initWorld
          case eRes of
            Left err -> do
              Utils.putRed $ "Failed to write world state to Database: " <> show err
              System.Exit.die ""
            Right _ -> pure ()

--------------------------------------------------------------------------------

-- | Load a pre-existing genesis block
loadBlocks
  :: DB.MonadReadDB m
  => (m (Either Text Block.Block) -> IO (Either Text Block.Block))
  -> IO (Block.Block, Block.Block)
loadBlocks runDB = do
  eGenesisBlock <- runDB $ DB.readBlock 0
  case eGenesisBlock of
    Left err -> do
      Utils.putRed $ "Failed to read genesis block from Database: " <> show err
      System.Exit.die "Database not intialized."
    Right gBlock -> do
      eLastBlock <- runDB DB.readLastBlock
      case eLastBlock of
        Left err -> do
          Utils.putRed $ "Failed to read last block from Database: " <> show err
          System.Exit.die "Database not intialized."
        Right lastBlock ->
          pure (gBlock,lastBlock)

-- | Initialize a Genesis Block and write it to the DB
initGenesisBlock
  :: DB.MonadWriteDB m
  => Config
  -> (m () -> IO ())
  -> IO ()
initGenesisBlock Config{..} runDB = do
  Log.info "Intializing genesis block..."
  cs@ChainSettings{..} <- handleChain False chainConfigFile
  case CAP.mkGenesisPoA poaValidators poaBlockPeriod poaBlockLimit poaSignLimit poaThreshold poaTransactions of
    Left err -> System.Exit.die $
      "Invalid PoA Consensus configuration: " <> show err
    Right genPoa -> do
      gBlock <- Block.genesisBlock (toS genesisHash) genesisTimestamp genPoa
      eRes <- try $ runDB $ DB.writeBlock gBlock
      case eRes of
        Left (err :: DB.PostgreSQL.SqlError) -> do
          Utils.putRed $ "Failed to write genesis block to database: " <> show err
          System.Exit.exitFailure
        Right _ -> pure ()

--------------------------------------------------------------------------------
-- Uplink
--------------------------------------------------------------------------------

-- | Main node entry
driver
  :: Command -- ^ Node configuration or file to compile
  -> Opts
  -> Config
  -> IO ()
driver (Chain cmd) opts (config @ Config {..}) = do

  Log.info (toS (Utils.ppShow config))

  -- Setup entropy pool with 4096 bytes of entropy
  -- ========================================
  pool <- Entropy.createEntropyPool
  seed <- Entropy.getEntropyFrom pool 128 :: IO ByteString

  -- Setup Logging
  -- =============
  let logLevel = if verbose then "DEBUG" else loggingLevel
  Log.configureLogging logLevel logfile

  when nonetwork $ do
    Log.info "Not connecting because --no-network"
    System.Exit.exitSuccess

  -- Setup Node Files Directory
  -- =========================
  createDirectoryIfMissing False nodeDataDir
  writeFile (TxLog.txLogFile nodeDataDir) mempty

   -- Setup Account
  ((nodeAcc, nodeKeys),nodeAccType) <-
    case cmd of
      Init accPrompt -> do
        eAccAndKeys <-
          Account.setupAccount
            nodeDataDir
            (Opts._privKey opts)
            accPrompt
        case eAccAndKeys of
          Left err -> do
            Utils.putRed $ toS err
            System.Exit.die $ toS err
          Right accAndKeys -> return (accAndKeys, NodeState.New)

      Run  -> do
        eAcc <- Account.loadAccount nodeDataDir
        case eAcc of
          Left err  -> do
            Utils.putRed $ toS err
            newAccAndKeys <-
              Account.setupAccount'
                nodeDataDir
                (Opts._privKey opts)
                Account.Prompt
            return (newAccAndKeys, NodeState.New)
          Right accAndKeys -> return (accAndKeys, NodeState.Existing)

  Utils.putGreen $ mconcat
    [ "Found existing account with fingerprint:\n    "
    , toS (Key.fingerprint $ Account.publicKey nodeAcc)
    ]


  -- Initialize NodeEnv (NodeConfig & NodeState)
  -- ===========================================
  let mkNodeEnv world genesisBlock lastBlock = do
        let eInvalidTxPool = MemPool.mkInvalidTxPool 100
        case eInvalidTxPool of
          Left err ->
            System.Exit.die
              "Invalid size specified for Invalid Transaction pool."
          Right invalidTxPool -> do
            nodeState <-
              NodeState.initNodeState
                world
                Set.empty
                MemPool.emptyMemPool
                invalidTxPool
                CAS.defPoAState
                lastBlock
            let nodeConfig =
                  NodeState.NodeConfig {
                    NodeState.account      = nodeAcc
                  , NodeState.nodePrivKey  = snd nodeKeys
                  , NodeState.accountType  = nodeAccType
                  , NodeState.genesisBlock = genesisBlock
                  , NodeState.config       = config
                  }
            pure $ NodeState.NodeEnv nodeConfig nodeState


  -- Run RPC Server & boot P2P processes:
  --   Dispatches on backend, using different MonadDB runner
  --   depending on the backend specified by the user in Config
  -- ==========================================================

  -- Channel for RPC server to send messages to P2P server
  rpcToP2PChan <- Chan.newChan

  case storageBackend of

    -- PostgreSQL Backend
    -----------------------
    DB.PostgreSQL connInfo -> do

      case cmd of

        -- `uplink chain init`
        Init _ -> do

          -- Attempt to create Uplink Postgres DB
          eRes <- DB.PostgreSQL.setupDB connInfo

          case eRes of
            Left err -> do
              Utils.putRed $ "Failed to create Uplink database: " <> show err
              System.Exit.die "Database initialization failed."

            Right _ -> do
              -- Connect to newly created Uplink Postgres Database
              connPool <- DB.PostgreSQL.connectDB connInfo
              let runPostgresT'= DB.PostgreSQL.runPostgresT connPool

              -- Initialize Genesis Block & World with Validator Accounts
              -- XXX Cleanup after failed initialization of either of these (`bracket`)
              initGenesisBlock config runPostgresT'
              initWorld config runPostgresT'

              Utils.putGreen "Successfully intialized Uplink Postgres Database."
              DB.PostgreSQL.closeConnPool connPool

        -- `uplink chain`
        Run -> pure ()

      -- Connect to Existing Uplink Postgres Database
      connPool <- DB.PostgreSQL.connectDB connInfo
      let runPostgresT'= DB.PostgreSQL.runPostgresT connPool


      -- Load world state and genesis block to initialize NodeEnv
      Utils.putGreen "Loading world state from DB."
      world <- loadWorld runPostgresT'
      Utils.putGreen "Loading genesis block from DB."
      (genesisBlock, lastBlock) <- loadBlocks runPostgresT'
      nodeEnv <- mkNodeEnv world genesisBlock lastBlock
      let runNodeT' = NodeState.runNodeT nodeEnv

      -- Launch RPC Server
      Utils.putGreen "Starting RPC server..."
      forkIO $ RPC.rpcServer config rpcToP2PChan $
        runPostgresT' . runNodeT'

      -- Launch P2P Server
      Utils.putGreen "Starting P2P processes..."
      P2P.p2p config rpcToP2PChan $
        runPostgresT' . runNodeT'

    -- LevelDB Backend
    -----------------------
    DB.LevelDB path -> do
      eDatabase <- DB.LevelDB.setupDB path
      case eDatabase of
        Left err -> do
          Utils.putRed $ toS err
          System.Exit.die $ toS err
        Right database -> do

          let runLevelDBT' = DB.LevelDB.runLevelDBT database

          case cmd of

            Init _ -> do

              -- Initialize Genesis Block & World with Validator Accounts
              -- XXX Cleanup after failed initialization of either of these (`bracket`)
              initGenesisBlock config runLevelDBT'
              initWorld config runLevelDBT'

              Utils.putGreen "Successfully intialized Uplink LevelDB Database."

            Run -> pure ()

          -- Load world state and genesis block to initialize NodeEnv
          world <- loadWorld runLevelDBT'
          (genesisBlock, lastBlock) <- loadBlocks runLevelDBT'
          nodeEnv <- mkNodeEnv world genesisBlock lastBlock
          let runNodeT' = NodeState.runNodeT nodeEnv

          -- Launch RPC Server
          Utils.putGreen "Starting RPC server..."
          forkIO $ RPC.rpcServer config rpcToP2PChan $
            runLevelDBT' . runNodeT'

          -- Launch P2P Server
          Utils.putGreen "Starting P2P processes..."
          P2P.p2p config rpcToP2PChan $
            runLevelDBT' . runNodeT'

driver (Script cmd) opts (config @ Config {..}) =
  case storageBackend of

    DB.PostgreSQL connInfo -> do
      connPool <- DB.PostgreSQL.connectDB connInfo
      DB.PostgreSQL.runPostgresT connPool $
        driverScript cmd opts config

    DB.LevelDB path -> do
      eDatabase <- DB.LevelDB.setupDB path
      case eDatabase of
        Left err -> do
          Utils.putRed $ toS err
          System.Exit.die $ toS err
        Right database ->
          DB.LevelDB.runLevelDBT database $
            driverScript cmd opts config

  -- Create authority accounts
driver (Keys (Opts.CreateAuthorities nAuths)) opts config =
  Authority.authorityDirs "auth" nAuths

driver Version opts config = pass

driver (Data cmd) opts (config @ Config {..}) = do
  case storageBackend of

    DB.PostgreSQL connInfo -> do
      connPool <- DB.PostgreSQL.connectDB connInfo
      DB.PostgreSQL.runPostgresT connPool $
        driverData cmd opts config

    DB.LevelDB path -> do
      eDatabase <- DB.LevelDB.setupDB path
      case eDatabase of
        Left err -> do
          Utils.putRed $ toS err
          System.Exit.die $ toS err
        Right database ->
          DB.LevelDB.runLevelDBT database $
            driverData cmd opts config


-- XXX Refactor using MonadReadWriteDB and MonadProcessBase
driver Console opts config = do
  let host = Config.hostname config
  let port = Config.port config
  remote <- mkNodeId $ toS $ host <> ":" <> show port

  localNode <-
    P2P.createLocalNode
      "localhost"
      ""
      (Just DPN.initRemoteTable)

  runProcess localNode $ console remote
  where
    console :: NodeId -> Process ()
    console remote = do

      -- Find the ProcessId for the tasks proc on the remote node
      (Just remoteTasksProc) <- findTasksProc remote
      link remoteTasksProc

      -- A CH channel to initially send alongside the cmd, for the remote node
      -- to send the response through
      resultChan <- newChan

      -- A channel which the console uses to send a command here
      -- so it can then be sent to the remote node
      localChan <- liftIO Chan.newChan

      -- A channel to send the result of a command to the console
      localResultChan <- liftIO Chan.newChan

      spawnLocal $ loop localChan localResultChan resultChan remoteTasksProc

      liftIO $ Console.runConsole $ Console.ConsoleConfig localChan localResultChan

    -- Handles requests/results for the console repl
    loop :: Chan.Chan Cmd.Cmd -> Chan.Chan Cmd.CmdResult -> (SendPort Cmd.CmdResult, ReceivePort Cmd.CmdResult) -> ProcessId -> Process ()
    loop localChan localResultChan (sport, rport) remoteTasksProc = forever $ do
      -- Get cmd from console repl
      cmd <- liftIO $ Chan.readChan localChan

      -- Send the cmd alongside a CH channel
      -- which the remote node sends the result through
      send remoteTasksProc (cmd, sport)

      -- Receive the result from the remote node
      -- and send to the console repl
      res <- receiveChan rport
      liftIO $ Chan.writeChan localResultChan res

    findTasksProc :: NodeId -> Process (Maybe ProcessId)
    findTasksProc node = do
      whereisRemoteAsync node tasks
      reply <- expectTimeout 1500000
      case reply of
        Just (WhereIsReply tasks (Just pid)) ->
          return $ Just pid -- XXX
        _ -> do
          putText $ "Failed to connect to: " <>  show node
          findTasksProc node

    tasks = show Service.Tasks

-------------------------------------------------------------------------------
-- Script Commands
-------------------------------------------------------------------------------

-- | CMD line driver for FCL repl
driverScript
  :: DB.MonadReadDB m
  => ScriptCommand
  -> Opts
  -> Config
  -> m ()
driverScript cmd opts (config @ Config {..}) =
  case cmd of
    -- lint
    Opts.Lint scriptFile -> liftBase $ do
      res <- Compile.lintFile scriptFile
      case res of
        [] -> System.Exit.exitSuccess
        xs -> do
          putStrLn (A.encodePretty xs)
          System.Exit.exitFailure

    -- format
    Opts.Format scriptFile -> liftBase $ do
      res <- Compile.formatScript scriptFile
      case res of
        Left err -> putStrLn err
        Right script -> putStrLn script

    -- graph
    Opts.Graph scriptFile -> liftBase $ do
      res <- Compile.compileFile scriptFile
      case res of
        Left err -> putStrLn err
        Right (_, script) -> do
          Utils.putGreen "Definitions:"
          putStrLn $ Pretty.printList (Script.scriptTransitions script)

          Utils.putGreen "Transitions:"
          Utils.ppDump $ Anal.transitions script

          Utils.putGreen "Branch Points:"
          Utils.ppDump $ Anal.branches script

          Utils.putGreen "Choice Points:"
          Utils.ppDump $ Anal.choices (Anal.makeGraph script)

          Anal.testVis (Anal.makeGraph script)

    -- compile
    Opts.CompileScript scriptFile localStorageFile -> liftBase $ do
      eSigs <- Compile.compileFile scriptFile
      case eSigs of
        Left err -> putText err
        Right (sigs, script) -> do
          putText "Signatures:"
          forM_ sigs $ \(name,sig) ->
            putText $ Pretty.prettyPrint name
              <> ": " <> Pretty.prettyPrint (Typecheck.ppSig sig)

          case localStorageFile of
            (Just path) -> do
              storage <- Compile.loadStorageFile path
              case storage of
                Left err -> putStrLn err
                Right store -> do
                  case Compile.verifyStorage store script of
                    Right _ -> putText "Local Storage is valid"
                    Left errs -> do
                      putText "Mismatched types in local storage:"
                      putText (show errs)

                  return ()
            _ -> return ()


    -- repl
    Opts.ReplScript mscriptFile verbose -> do
      eScript <- liftBase $ case mscriptFile of
        Just scriptFile -> Compile.compileFile scriptFile
        Nothing         -> Compile.emptyTarget

      case eScript of
        Left err -> liftBase $ putText err
        Right (sigs, script) -> do -- prepare context, load repl, print sigs
          liftBase $ do
            putText "Signatures:"
            forM_ sigs $ \(name,sig) -> do
              putText $ Pretty.prettyPrint name
                <> ": " <> Pretty.prettyPrint (Typecheck.ppSig sig)
          Repl.repl nodeDataDir sigs script verbose

-------------------------------------------------------------------------------
-- Data Commands
-------------------------------------------------------------------------------

driverData
  :: DB.MonadReadWriteDB m
  => DataCommand
  -> Opts
  -> Config
  -> m ()
driverData cmd opts (config @ Config {..}) =
  case cmd of
    Opts.Commit path contractAddr accountAddr -> do
        storage <- liftBase $ Compile.loadStorageFile path
        case storage of
          Left err -> liftBase $ putStrLn err
          Right store -> do
            contract <- DB.readContract contractAddr
            liftBase $ case contract of
              Right c -> do
                case Compile.verifyStorage store (Contract.script c) of
                    Right _ -> putText "Local Storage is valid"
                    Left errs -> do
                      putText "Mismatched types in local storage:"
                      putText (show errs)

                let ls = Map.insert accountAddr (Storage.LocalStorage store) (Contract.localStorage c)
                let c' = c { Contract.localStorage = ls }
                (params, cmt) <- Commit.commitTo store
                putText $ show $ cmt

              _ -> putText "Contract not found"
            return ()

    Opts.Get addr -> do
      liftBase $
        Utils.putGreen (Pretty.prettyPrint addr)

      account  <- DB.readAccount addr
      asset    <- DB.readAsset addr
      contract <- DB.readContract addr

      liftBase $ do
        case account of
          Right v -> display v
          _ -> return ()
        case asset of
          Right v -> display v
          _ -> return ()
        case contract of
          Right v -> do
              display v
              display (Contract.localStorage v)
          _ -> return ()

    Opts.List -> do
      account  <- DB.readAccounts
      asset    <- DB.readAssets
      contract <- DB.readContracts
      block    <- DB.readBlocks

      liftBase $ do
        Utils.putGreen "Accounts: "
        case account of
          Right v -> display v
          _ -> return ()

        Utils.putGreen "Assets: "
        case asset of
          Right v -> display v
          _ -> return ()

        Utils.putGreen "Contracts: "
        case contract of
          Right v -> display v
          _ -> return ()

        Utils.putGreen "Blocks: "
        case block of
          Right v -> display v
          _ -> return ()

    Opts.LoadAsset fp addr -> do
      blob <- liftBase $ BSL.readFile fp
      case A.decode blob of
        Nothing -> Log.info "Could not read Asset JSON"
        Just asset -> do
          ledger <- DB.readWorld
          let world = do
                world <- ledger
                first show $
                  Ledger.addAsset addr asset world
          case world of
            Left err -> Log.info $ toS $ "Load Asset: " <> err
            Right world' -> do
              DB.writeAsset asset
              eRes <- DB.syncWorld world'
              case eRes of
                Left err -> Log.warning $ show err
                Right _  -> pure ()

    Opts.LoadAccount fp -> do
      blob <- liftBase $ BSL.readFile fp
      case A.decode blob of
        Nothing -> Log.info "Could not read Account JSON"
        Just acc -> do
          ledger <- DB.readWorld
          let world = do
                world <- ledger
                first show $ Ledger.addAccount acc world
          case world of
            Left err -> Log.info $ toS $ "Load Asset: " <> err
            Right world' -> do
              DB.writeAccount acc
              eRes <- DB.syncWorld world'
              case eRes of
                Left err -> Log.warning $ show err
                Right _  -> pure ()

    Opts.Export fp -> do
      blocks <- DB.readBlocks
      liftBase $
        case blocks of
          Right v -> BSL.writeFile fp (XML.toXML v)
          _ -> putText "Could not read database"
  where
    display v = putStrLn $ A.encodePretty v
