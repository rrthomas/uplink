{-

Driver for command line option handling.

-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

module Driver (
  Command(..),
  driver,
  driverChain,
) where

import Protolude hiding (link, newChan)

import Control.Monad.Base
import Control.Distributed.Process hiding (try)
import qualified Control.Distributed.Process.Node as DPN
import Control.Distributed.Process.Node (runProcess)

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Aeson as A
import qualified Data.Coerce as Coerce

import Opts
import Config (Config(..), ChainSettings(..), handleChain)
import qualified RPC
import qualified Key
import qualified Utils
import qualified Ledger
import qualified Script
import qualified Commit
import qualified Account
import qualified Console
import qualified Console.Config
import qualified NodeState
import qualified Node.Files as NF
import qualified Authority
import qualified MemPool
import qualified Contract
import qualified Storage
import qualified Consensus.Authority.State  as CAS
import qualified Script.Pretty as Pretty
import qualified Script.Analysis as Anal
import qualified Script.Compile as Compile
import qualified Script.Typecheck as Typecheck
import qualified System.Console.ANSI as ANSI
import qualified System.Exit

import qualified DB
import qualified DB.PostgreSQL
import qualified DB.LevelDB

import Network.Utils
import qualified Network.P2P as P2P
import qualified Network.P2P.Service as S
import qualified Network.P2P.Cmd as Cmd
import qualified Network.P2P.Simulate as Sim
import qualified Network.Transport as NT

import qualified XML
import qualified REPL

import SafeString

#ifdef EKG
import System.Remote.Monitoring (forkServer)
#endif

--------------------------------------------------------------------------------
-- Uplink
--------------------------------------------------------------------------------

-- | Main node entry
driverChain
  :: ChainCommand                               -- ^ New or Existing
  -> Config                                     -- ^ Node Configuration
  -> Maybe (MVar (DPN.LocalNode, NT.Transport)) -- ^ Testing purposes; Should most often be 'Nothing'
  -> IO ()
driverChain cmd (config @ Config {..}) mNodeAndTpSink = do

  logInfo (toS (Utils.ppShow config))

  -- Read/Validate Chain config
  -- ==========================
  chainSettings <- handleChain False chainConfigFile

  when nonetwork $ do
    logInfo "Not connecting because --no-network"
    System.Exit.exitSuccess

  -- Read Network Access Token
  -- =========================
  accessToken <- do
    eAccessToken <- Key.readKeys (Config.accessToken config)
    case eAccessToken of
      Left err -> Utils.dieRed $
        "Failed to read network access token: " <> err
      Right atok -> pure atok

  -- Setup Node Data Files
  -- =====================
  (nodeData,nodeAccType) <- do
    (eNodeData, accType) <-
      case cmd of
        Init accPrompt _ -> do
          -- Attempt to read existing node data from nodeDataDir
          eNodeData <- NF.readNodeDataDir nodeDataDir
          case eNodeData of
            Right _  -> do -- Fail if node data already exists
              Utils.putRed $ "Node data files already exist at '" <> toS nodeDataDir <> "'."
              Utils.dieRed "Please specify a different directory with the '-d' flag."
            Left err -> do
              (acc,keys) <-
                Account.createAccPrompt' accPrompt
              -- Initialize node data directory
              fmap (,NodeState.New) $
                NF.initNodeDataDir nodeDataDir acc keys >>= \enfps ->
                  either (pure . Left) NF.readNodeDataFiles enfps
        Run -> (,NodeState.Existing) <$> NF.readNodeDataDir nodeDataDir

    case eNodeData of
      Left err       -> Utils.dieRed $ toS err
      Right nodeData -> pure (nodeData, accType)

  let nodeDataFps = NF.mkNodeDataFilePaths nodeDataDir
  let fingerprint = toS $ Key.fingerprint (fst $ NF.nodeKeys nodeData)
  Utils.putGreen $ "Found existing account with fingerprint:\n  " <> fingerprint

  -- Initialize NodeEnv (NodeConfig & NodeState)
  -- ===========================================
  let initNodeEnv initWorld genesisBlock lastBlock = do
        eNodeEnv <- do
          let testMode = Config.testMode config
              preallocated = Config.preallocated config
              bootnodeBSs  = Config.bootnodes config
              loggerRules = Config.loggingRules config
          (errs, bootnodeIds) <- partitionEithers <$> mapM mkNodeId bootnodeBSs
          -- Log invalid boot nodes
          mapM_ Utils.putRed errs
          NodeState.initNodeEnv
            nodeData nodeDataFps nodeAccType bootnodeIds genesisBlock preallocated testMode accessToken loggerRules -- NodeConfig args
            initWorld Set.empty MemPool.emptyMemPool 100 CAS.defPoAState lastBlock -- NodeState args
        case eNodeEnv of
          Left err -> Utils.dieRed $ "Failed to intialize Uplink node environment: " <> show err
          Right nodeEnv -> pure nodeEnv

  -- Fork an HTTP monitoring server for an uplink node using the
  -- build invocation: `stack build --flag uplink:-ekg`
  -- ===========================================================
#ifdef EKG
  -- Fork a monitoring server to monitor uplink when building with stack
  Utils.putGreen $ "Forking a monitoring server on port: " <> show monitorPort
  forkServer "localhost" monitorPort
#endif

  -- Run RPC Server & boot P2P processes:
  --   Dispatches on backend, using different MonadDB runner
  --   depending on the backend specified by the user in Config
  -- ==========================================================
  case storageBackend of

    -- PostgreSQL Backend
    -----------------------
    DB.PostgreSQL connInfo -> do

      -- Connect to Existing Uplink Postgres Database
      eConnPool <- DB.PostgreSQL.tryConnectDB connInfo
      connPool <-
        case eConnPool of
          Left err ->
            case cmd of

              -- If initializing database and failed to connect to existing
              -- database, try to setup database and import data if specified.
              Init _ mImport -> do
                -- Attempt to create Uplink Postgres DB
                let dbName = toS (DB.PostgreSQL.connectDatabase connInfo)
                Utils.putGreen $ "Creating Uplink database \"" <> dbName <> "\"..."
                eConnPool' <- DB.PostgreSQL.setupDB connInfo
                case eConnPool' of
                  Left err -> Utils.dieRed $
                    "Failed to create Uplink database: " <> show err
                  Right connPool -> do
                    let runPostgresT' = DB.PostgreSQL.runPostgresT connPool
                    handleJSONDataImport chainSettings runPostgresT' mImport
                    pure connPool

              -- If running with existing database, but failed to connect, die.
              Run -> Utils.dieRed $ "Failed to create Uplink database: " <> show err

          Right connPool ->
            case cmd of
              Init _ _ -> do
                let dbName = toS (DB.PostgreSQL.connectDatabase connInfo)
                Utils.dieRed $ "Uplink database with name \"" <> dbName <> "\" already exists."
              Run -> pure connPool

      let runPostgresT'= DB.PostgreSQL.runPostgresT connPool

      -- Load world state and genesis block to initialize NodeEnv
      Utils.putGreen "Loading world state from DB."
      world <- DB.loadWorld runPostgresT'
      Utils.putGreen "Loading genesis block from DB."
      (genesisBlock, lastBlock) <- DB.loadBlocks runPostgresT'
      nodeEnv <- initNodeEnv world genesisBlock lastBlock
      let runNodeT' = NodeState.runNodeT nodeEnv

      -- Create the uplink local node to run the services on and reify the main
      -- uplink process that spawns all services.
      let runToProc = runPostgresT' . runNodeT'
      (localNode, uplinkProc) <- createLocalNodeAndUplinkProc runToProc

      -- Launch RPC Server
      Utils.putGreen "Starting RPC server..."
      forkIO $ RPC.rpcServer config runToProc localNode (Just connPool)

      -- Launch P2P Server
      Utils.putGreen "Starting P2P processes..."
      runProcess localNode . runToProc $
        P2P.runUplink uplinkProc

    -- LevelDB Backend
    -----------------------
    DB.LevelDB path -> do

      levelDB <-
        case cmd of

          Init _ mImport -> do
            Utils.putGreen $ "Creating LevelDB on path: " <>  toS path
            eLevelDB <- DB.LevelDB.createDB path
            case eLevelDB of
              Left err -> Utils.dieRed $
                "Couldn't create LevelDB at path '" <> toS path <> ":\n  " <> toS err
              Right db -> do
                Utils.putGreen "Run levelDB"
                let runLevelDBT' = DB.LevelDB.runLevelDBT db
                handleJSONDataImport chainSettings runLevelDBT' mImport
                pure db

          Run -> do
            eLevelDB <- DB.LevelDB.loadExistingDB path
            case eLevelDB of
              Left err -> Utils.dieRed $
                "Couldn't load LevelDB at path '" <> toS path <> ":\n  " <> toS err
              Right db -> do
                logInfo $ "Using existing LevelDB database at: " <> toS path
                pure db

      let runLevelDBT' = DB.LevelDB.runLevelDBT levelDB

      -- Load world state and genesis block to initialize NodeEnv
      Utils.putGreen "Loading world state from DB."
      world <- DB.loadWorld runLevelDBT'
      Utils.putGreen "Loading genesis block from DB."
      (genesisBlock, lastBlock) <- DB.loadBlocks runLevelDBT'
      nodeEnv <- initNodeEnv world genesisBlock lastBlock
      let runNodeT' = NodeState.runNodeT nodeEnv

      -- Create the uplink local node to run the services on and reify the main
      -- uplink process that spawns all services.
      let runToProc = runLevelDBT' . runNodeT'
      (localNode, uplinkProc) <- createLocalNodeAndUplinkProc runToProc

      -- Launch RPC Server
      Utils.putGreen "Starting RPC server..."
      forkIO $ RPC.rpcServer config runToProc localNode Nothing

      -- Launch P2P Server
      Utils.putGreen "Starting P2P processes..."
      runProcess localNode . runToProc $
        P2P.runUplink uplinkProc

  where

    createLocalNodeAndUplinkProc runToProc =
      case mNodeAndTpSink of
        Nothing -> P2P.createLocalNodeFromService config P2P.Uplink runToProc
        Just sink -> do
           -- This is for testing purposes only:
           -- Tbe caller of the 'driver' function sometimes needs access to the
           -- LocalNode created, as well as the Transport it was created with
          (localNode, uplinkProc, transport) <-
            P2P.createLocalNodeFromService' transport hostname (show port) P2P.Uplink runToProc
          tryPutMVar sink (localNode, transport)
          pure (localNode, uplinkProc)

    loadBlocksFromJSON fp = do
      eBlocksJSON <- Utils.safeReadLazy fp
      case eBlocksJSON of
        Left err -> Utils.dieRed err
        Right blocksJSON ->
          case A.eitherDecode blocksJSON of
            Left err   -> Utils.dieRed $
              "Failed to decode Uplink blocks from JSON:\n   " <> toS err
            Right blks -> pure blks

    -- If I put the typesig to this function it can't compile :(
    handleJSONDataImport ChainSettings{..} runDB = \case
      -- In the case that we are not importing data...
      Nothing -> do
        -- Initialize Genesis Block & World with Validator Accounts
        logInfo "Intializing genesis block..."
        DB.initGenesisBlock genesisHash genesisTimestamp genesisPoA runDB
        DB.initWorld preallocated runDB
        Utils.putGreen "Successfully intialized Uplink Database."

      -- In the case that we're importing blocks
      Just (ImportBlocks fp) -> do
        logInfo $ "Importing blocks from JSON file: " <> toS fp
        blocks <- loadBlocksFromJSON fp
        case blocks of
          []     -> Utils.dieRed "No blocks to load..."
          (b:bs) -> do
            -- Initialize the world
            DB.initWorld preallocated runDB
            -- Apply the imported blocks to the ledger state...
            eRes <- runDB $ DB.writeBlocks (b:bs)
            case eRes of
              Left err -> Utils.dieRed $
                "Failed to write imported blocks to the database:\n   " <> show err
              Right () -> pure ()

      -- In the case that we're importing the ledger state (for testing)
      Just (ImportLedger fp) -> do
        ledgerE <- Ledger.loadLedgerFromJSON fp
        case ledgerE of
          Left err -> Utils.dieRed err
          Right ledger -> do
            -- Write to the ledger with the data from the file
            logInfo "Initializing ledger state from JSON import..."
            DB.initWorld preallocated runDB
            eRes <- runDB $ DB.syncWorld ledger
            case eRes of
              Left err -> Utils.dieRed $ show err
              Right _  -> do
                -- Since no blocks were written, intialize genesis block
                logInfo "Intializing genesis block..."
                DB.initGenesisBlock genesisHash genesisTimestamp genesisPoA runDB

driver :: Command -> Config -> IO ()
driver (Chain cmd) config =
  driverChain cmd config Nothing

driver (Script cmd) config =
  driverScript cmd config

-- Create authority accounts
driver (Keys (Opts.CreateAuthorities nAuths)) config =
  Authority.authorityDirs "auth" nAuths

driver Version config = pass

driver (Data cmd) (config @ Config {..}) =
  case storageBackend of

    DB.PostgreSQL connInfo -> do
      eConnPool <- DB.PostgreSQL.tryConnectDB connInfo
      case eConnPool of
        Left err -> Utils.dieRed $
          "Failed to create Uplink database: " <> show err
        Right connPool ->
          DB.PostgreSQL.runPostgresT connPool $
            driverData cmd config

    DB.LevelDB path -> do
      eDatabase <- DB.LevelDB.loadExistingDB path
      case eDatabase of
        Left err -> Utils.dieRed $ toS err
        Right database ->
          DB.LevelDB.runLevelDBT database $
            driverData cmd config


-- XXX Refactor using MonadReadWriteDB and MonadProcessBase
driver Console config@Config{..} = do
    eRemote <- mkNodeId $ toS $ hostname <> ":" <> show port
    localNode <- P2P.createLocalNode' transport "localhost" "" (Just DPN.initRemoteTable)
    case eRemote of
      Left err -> Utils.dieRed err
      Right remote -> runConsoleProcs localNode remote
  where
    runConsoleProcs :: DPN.LocalNode -> NodeId -> IO ()
    runConsoleProcs localNode cmdProcNodeId = do

      -- Find the ProcessId for the tasks proc on the remote node
      cmdProcIdMVar <- newEmptyMVar
      runProcess localNode $ do
        mCmdProcId <- S.findRemoteServiceTimeout cmdProcNodeId Cmd.ExternalCmd 1000000
        case mCmdProcId of
          Nothing -> liftIO $ Utils.dieRed "Failed to find process..."
          Just cmdProcId -> do
            link cmdProcId
            liftIO $ putMVar cmdProcIdMVar cmdProcId

      -- Run the console process
      -- XXX Initialize console state with account & privKey, read from disk
      remoteCmdProcId <- takeMVar cmdProcIdMVar
      Console.runConsole localNode remoteCmdProcId
        Console.Config.ConsoleState
          { account = Nothing
          , privKey = Nothing
          , vars    = mempty
          }

driver (Repl path verbose mWorldPath) config@Config{..} = do
      eRemote <- mkNodeId $ toS $ hostname <> ":" <> show port
      localNode <- P2P.createLocalNode' transport "localhost" "" (Just DPN.initRemoteTable)
      case eRemote of
        Left err -> Utils.dieRed err
        Right remote -> runReplProcs localNode remote
    where
      runReplProcs :: DPN.LocalNode -> NodeId -> IO ()
      runReplProcs localNode simProcNodeId = do

        logInfo $ "Searching for simulation process on: " <> show simProcNodeId
        -- Find the ProcessId for the Simulation proc on the remote node
        simProcIdMVar <- newEmptyMVar
        runProcess localNode $ do
          mSimProcId <- S.findRemoteServiceTimeout simProcNodeId Sim.Simulation 1000000
          case mSimProcId of
            Nothing -> liftIO $ Utils.dieRed "Failed to find simulation process..."
            Just simProcId -> do
              link simProcId
              liftIO $ putMVar simProcIdMVar simProcId

        remoteSimProcId <- takeMVar simProcIdMVar

        logInfo $ "Reading script from: " <> show path
        scriptE <- Utils.safeRead path

        -- Try and load a ledger state from 'uplink data export ledger <path>', if a path supplied
        mWorld <- case mWorldPath of
          Nothing -> pure Nothing
          Just fp -> do

            logInfo $ "Loading ledger state: " <> show fp
            l <- Ledger.loadLedgerFromJSON fp
            case l of
              Left err -> Utils.dieRed err
              Right w -> pure $ Just w

        case scriptE of
          Right script ->
            REPL.runRepl localNode remoteSimProcId (fromBytes' script) mWorld verbose
          Left err -> putText err

-------------------------------------------------------------------------------
-- Script Commands
-------------------------------------------------------------------------------

-- | CMD line driver for FCL repl
driverScript
  :: ScriptCommand
  -> Config
  -> IO ()
driverScript cmd (config @ Config {..}) =
  case cmd of
    -- lint
    Opts.Lint scriptFile -> do
      res <- Compile.lintFile scriptFile
      case res of
        [] -> System.Exit.exitSuccess
        xs -> do
          putStrLn (A.encodePretty xs)
          System.Exit.exitFailure

    -- format
    Opts.Format scriptFile -> do
      res <- Compile.formatScript scriptFile
      case res of
        Left err -> putStrLn err
        Right script -> putStrLn script

    -- graph
    Opts.Graph scriptFile -> do
      res <- Compile.compileFile scriptFile
      case res of
        Left err -> putStrLn err
        Right (_, script) -> do
          Utils.putGreen "Definitions:"
          putStrLn $ Pretty.printList (Script.scriptTransitions script)

          Utils.putGreen "Valid transitions:"
          Utils.ppDump $ Anal.validTransitions script

          Utils.putGreen "Branch Points:"
          Utils.ppDump $ Anal.branches script

          Utils.putGreen "Choice Points:"
          Utils.ppDump $ Anal.choices (Anal.makeGraph script)

          Anal.testVis script

    -- compile
    Opts.CompileScript scriptFile localStorageFile -> do
      eSigs <- Compile.compileFile scriptFile
      case eSigs of
        Left err -> putText err
        Right (sigs, script) -> do
          putText "Signatures:"
          forM_ sigs $ \(name,sig,effect) ->
            putText $ Pretty.prettyPrint name
              <> ": " <> Pretty.prettyPrint (Typecheck.ppSig sig) <> " " <> Pretty.prettyPrint effect

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

-------------------------------------------------------------------------------
-- Data Commands
-------------------------------------------------------------------------------

driverData
  :: DB.MonadReadWriteDB m
  => DataCommand
  -> Config
  -> m ()
driverData cmd (config @ Config {..}) =
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
      asset    <- DB.readAsset (Coerce.coerce addr)
      contract <- DB.readContract (Coerce.coerce addr)

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
        Nothing -> logInfo "Could not read Asset JSON"
        Just asset -> do
          ledger <- DB.readWorld
          let world = do
                world <- first show ledger
                first show $ Ledger.addAsset addr asset world
          case world of
            Left err -> logInfo $ "Load Asset: " <> err
            Right world' -> do
              DB.writeAsset asset
              eRes <- DB.syncWorld world'
              case eRes of
                Left err -> logWarning $ show err
                Right _  -> pure ()

    Opts.LoadAccount fp -> do
      blob <- liftBase $ BSL.readFile fp
      case A.decode blob of
        Nothing -> logInfo "Could not read Account JSON"
        Just acc -> do
          ledger <- DB.readWorld
          let world = do
                world <- first show $ ledger
                first show $ Ledger.addAccount acc world
          case world of
            Left err -> logInfo $ "Load Asset: " <> err
            Right world' -> do
              DB.writeAccount acc
              eRes <- DB.syncWorld world'
              case eRes of
                Left err -> logWarning $ show err
                Right _  -> pure ()

    Opts.Export (ExportBlocks typ) fp -> do
      eBlocks <- DB.readBlocks
      liftBase $ case eBlocks of
        Right blocks ->
          case typ of
            XML  -> BSL.writeFile fp $ XML.toXML blocks
            JSON -> BSL.writeFile fp . toS $ A.encodePretty blocks
        Left err -> Utils.dieRed $ "Could not read database:\n   " <> show err
    Opts.Export ExportLedgerState fp -> do
      eWorld <- DB.readWorld
      liftBase $ case eWorld of
        Left err    -> Utils.dieRed $ "Could not read database:\n   " <> show err
        Right world -> BSL.writeFile fp $ toS $ A.encodePretty world

  where
    display v = putStrLn $ A.encodePretty v

--------------------------------------------------------------------------------
-- Logging
--------------------------------------------------------------------------------

-- Note that this module has its own logging functionality, as the
-- functions in this module are called before the logging process has
-- been spawned.

logInfo :: MonadBase IO m => Text -> m ()
logInfo = liftBase . putStrLn

logWarning :: MonadBase IO m => Text -> m ()
logWarning = liftBase . Utils.withColor ANSI.Yellow putStrLn
