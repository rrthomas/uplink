{-|

Driver for command line option handling.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Driver (
  Command(..),
  driver,
) where

import Protolude hiding (link, newChan)
import Opts
import Config (Config(..), ChainSettings(..), handleChain)
import qualified DB
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
import qualified Logging as Log
import qualified Network.P2P as Transport
import qualified Network.P2P.Cmd as Cmd
import qualified Consensus.Authority.Types as CAT
import qualified Script.Pretty as Pretty
import qualified Script.Analysis as Anal
import qualified Script.Compile as Compile
import qualified Script.Typecheck as Typecheck
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Aeson as A
import System.Directory
import qualified System.Exit
import qualified Control.Concurrent.Chan as Chan
import qualified Pedersen
import Crypto.Random.EntropyPool as Entropy
import Control.Distributed.Process
import qualified Control.Distributed.Process.Node as DPN
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import qualified Network.Transport.TCP as TCP
import qualified Network.P2P as P2P
import qualified Network.P2P.Service as Service
import Network.Utils
import qualified XML
import qualified Text.XML.Expat.Tree as XML
import qualified Text.XML.Expat.Pickle as XML
import qualified Text.XML.Expat.Format as XML

rpcToP2pChan :: IO (Chan.Chan Cmd.Cmd)
rpcToP2pChan = do
  Log.info "Setting up server comm."
  Chan.newChan

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


  -- Setup Database
  -- ==============
  let dbpath' = Config.dbpath config
  eDbPath <- case cmd of
    Init _ -> DB.setupNewDB dbpath'
    Run    -> DB.setupExistingDB dbpath'

  case eDbPath of
    Left err     -> System.Exit.die (toS err)
    Right dbpath -> do
      Utils.putGreen "Database is good."

      when nonetwork $ do
        Log.info "Not connecting because --no-network"
        System.Exit.exitSuccess


      -- Setup Account
      -- =============
      (nodeAcc,nodeAccType) <- case cmd of
        Init accPrompt -> do
          eAcc <- Account.setupAccount dbpath (Opts._privKey opts) accPrompt
          case eAcc of
            Left err -> do
              Utils.putRed $ toS err
              System.Exit.die $ toS err
            Right acc -> return (acc, NodeState.New)
        Run  -> do
          eAcc <- Account.loadAccount dbpath
          case eAcc of
            Left err  -> do
              Utils.putRed $ toS err
              newAcc <- Account.setupAccount' dbpath (Opts._privKey opts) Account.Prompt
              return (newAcc, NodeState.New)
            Right acc -> return (acc, NodeState.Existing)

      Utils.putGreen $ mconcat
        [ "Found existing account with fingerprint:\n    "
        , toS (Key.fingerprint $ Account.publicKey nodeAcc)
        ]

      -- Create Node State
      -- =================

      -- Load/open all DBs
      nodeDBs <- DB.loadDBs dbpath

      -- Load world state from DB
      -- XXX Revisit and do this in a better way?
      worldFromDB <- do
        eWorld <- DB.readWorld nodeDBs
        case eWorld of
          Left err -> do
            Utils.putRed "Could not read world state from DB."
            System.Exit.die "Database corrupt."
          Right w -> pure w

      -- XXX Validator account loading into Ledger for all nodes (used for testing)
      -- Should remove once validator account & key creation & distribution is solved
      validatorDir <- do
        let path1 = "config/validators/"
        let path2 = "/etc/uplink/validators/"
        p1Exists <-  doesDirectoryExist path1
        p2Exists <-  doesDirectoryExist path2
        if | p1Exists -> return path1
           | p2Exists -> return path2
           | otherwise ->
              System.Exit.die $ "No validator directory found\n Tried " <> path1 <> " and " <> path2

      eValidatorAccs <- do
        dirs <- listDirectory validatorDir
        when (null dirs) $
          System.Exit.die $ "Validator directory " <> validatorDir <> " is empty"

        forM dirs $
              \name -> Account.loadAccount (validatorDir ++ name)

      initWorld <- case partitionEithers eValidatorAccs of
        ([],accs) -> return $
          either (const worldFromDB) identity $ foldM (flip Ledger.addAccount) worldFromDB accs
        (errs,_) -> System.Exit.die $ "Error loading validator accounts: " ++ concatMap toS errs

      eKeys <- Account.readKeys dbpath
      privKey <- case eKeys of
        Left err -> System.Exit.die (toS err)
        Right (_,privKey') -> return privKey'

      -- Initialize Genesis Block
      eBlocks <- DB.allBlocks $ DB.blockDB nodeDBs
      case eBlocks of
        Left err -> System.Exit.die $ toS err
        Right blocks -> when (null blocks) $ do
          Log.info "Intializing genesis block..."
          cs@ChainSettings{..} <- handleChain False chainConfigFile
          case CAT.mkGenesisPoA cs of
            Left err -> System.Exit.die $
              "Invalid PoA Consensus configuration: " <> show err
            Right genPoa -> do
              gBlock <- Block.genesisBlock (toS genesisHash) genesisTimestamp genPoa
              DB.writeBlock (DB.blockDB nodeDBs) gBlock

      -- Initialize NodeState
      Right genesisBlock <- DB.lookupBlock (DB.blockDB nodeDBs) 0
      let nodeConfig = NodeState.NodeConfig {
              NodeState.account      = nodeAcc
            , NodeState.nodePrivKey  = privKey
            , NodeState.accountType  = nodeAccType
            , NodeState.genesisBlock = genesisBlock
            , NodeState.config       = config
            , NodeState.databases    = nodeDBs
            }
      nodeState <-
        NodeState.initNodeState
          initWorld
          Set.empty
          MemPool.emptyMemPool
          CAT.defPoAState
          genesisBlock

      -- Run RPC Server & boot P2P processes
      -- ===================================

      chan <- rpcToP2pChan

      Utils.putGreen "Starting RPC server..."
      forkIO $ RPC.rpcServer config chan nodeConfig nodeState

      Utils.putGreen "Starting P2P processes..."
      Transport.p2p chan nodeConfig nodeState

driver (Script cmd) opts (config @ Config {..}) =
  driverScript cmd opts config

  -- Create authority accounts
driver (Keys (Opts.CreateAuthorities nAuths)) opts config =
  Authority.authorityDirs "auth" nAuths

driver Version opts config = pass

driver (Data cmd) opts (config @ Config {..}) = do
  let dbpath' = Config.dbpath config
  eDbPath <- DB.setupExistingDB dbpath'
  case eDbPath of
    Left err     -> System.Exit.die (toS err)
    Right dbpath -> do
      nodeDBs <- DB.loadDBs dbpath
      driverData cmd opts config nodeDBs

driver Console opts config = do
  let host = Config.hostname config
  let port = Config.port config
  remote <- mkNodeId $ toS $ host <> ":" <> show port

  localNode <- P2P.createLocalNode "localhost" "" DPN.initRemoteTable -- XXX

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
  :: ScriptCommand
  -> Opts
  -> Config
  -> IO ()
driverScript cmd opts (config @ Config {..}) =
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

          Utils.putGreen "Transitions:"
          Utils.ppDump $ Anal.transitions script

          Utils.putGreen "Branch Points:"
          Utils.ppDump $ Anal.branches script

          Utils.putGreen "Choice Points:"
          Utils.ppDump $ Anal.choices (Anal.makeGraph script)

          Anal.testVis (Anal.makeGraph script)

    -- compile
    Opts.CompileScript scriptFile localStorageFile -> do
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
      eScript <- case mscriptFile of
        Just scriptFile -> Compile.compileFile scriptFile
        Nothing         -> Compile.emptyTarget

      case eScript of
        Left err -> putText err
        Right (sigs, script) -> do -- pepare context, load repl, print sigs
          putText "Signatures:"
          forM_ sigs $ \(name,sig) -> do
            putText $ Pretty.prettyPrint name
              <> ": " <> Pretty.prettyPrint (Typecheck.ppSig sig)

          -- Check for DB existence
          eDBs <- DB.setupExistingDB dbpath
          case eDBs of
            Left err -> System.Exit.die $
              "No DB exists:\n\t" <> toS err
            Right path -> do
              eNodeAddr <- fmap Account.address <$> Account.loadAccount dbpath
              case eNodeAddr of
                Left err -> System.Exit.die $ toS err
                Right nodeAddr -> do
                  Repl.repl path nodeAddr sigs script verbose

-------------------------------------------------------------------------------
-- Data Commands
-------------------------------------------------------------------------------

driverData
  :: DataCommand
  -> Opts
  -> Config
  -> DB.Databases
  -> IO ()
driverData cmd opts (config @ Config {..}) (databases @ DB.Databases {..}) =
  case cmd of
    Opts.Commit path contractAddr accountAddr -> do
        storage <- Compile.loadStorageFile path
        case storage of
          Left err -> putStrLn err
          Right store -> do
            contract <- DB.lookupContract contractDB contractAddr
            case contract of
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
      Utils.putGreen (Pretty.prettyPrint addr)
      account <- DB.lookupAccount accountDB addr
      asset <- DB.lookupAsset assetDB addr
      contract <- DB.lookupContract contractDB addr
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
      account <- DB.allAccounts accountDB
      asset <- DB.allAssets assetDB
      contract <- DB.allContracts contractDB
      block <- DB.allBlocks blockDB

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
      blob <- BSL.readFile fp
      case A.decode blob of
        Nothing -> Log.info "Could not read Asset JSON"
        Just asset -> do
          ledger <- DB.readWorld databases
          let world = do
                world <- ledger
                first show $ Ledger.addAsset addr asset world
          case world of
            Left err -> Log.info $ toS $ "Load Asset: " <> err
            Right world' -> do
              DB.writeAsset assetDB (addr, asset)
              DB.syncWorld databases world'

    Opts.LoadAccount fp -> do
      blob <- BSL.readFile fp
      case A.decode blob of
        Nothing -> Log.info "Could not read Account JSON"
        Just acc -> do
          ledger <- DB.readWorld databases
          let world = do
                world <- ledger
                first show $ Ledger.addAccount acc world
          case world of
            Left err -> Log.info $ toS $ "Load Asset: " <> err
            Right world' -> do
              DB.writeAccount accountDB acc
              DB.syncWorld databases world'
    Opts.Export fp -> do
      blocks <- DB.allBlocks blockDB
      case blocks of
        Right v -> BSL.writeFile fp (XML.toXML v)
        _ -> putText "Could not read database"
  where
    display v = putStrLn $ A.encodePretty v
