{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (
  main,
) where

import Protolude hiding ((<>), option)

import Data.Monoid ((<>))
import Data.List (unlines, words)
import qualified Data.ByteString.Char8 as BS

import System.IO (hSetBuffering, BufferMode(..))

import Options.Applicative

-- Application logic
import Opts hiding (verbose)
import qualified Utils
import qualified Config
import qualified Driver
import qualified Account
import qualified Version
import qualified Address

title :: [Char]
title = "Uplink" <> " " <> Version.version

-- Uses evil template Haskell to get git branch at compile-time.
banner :: [Char]
banner = unlines [
    title
  , "Copyright Adjoint Inc."
  , "Branch   : " <> Version.branch
  , "Revision : " <> Version.commit
  , "Dirty    : " <> Version.dirty
  ]

-------------------------------------------------------------------------------
-- Chain
-------------------------------------------------------------------------------

-- | Parse the options from the commandline.
chainParser :: Parser Opts
chainParser = runChainParser
          <|> initChainParser
  where
    runChainParser :: Parser Opts
    runChainParser = do
      chainOpts <- optsParser
      return chainOpts { _command = Driver.Chain Run }

    initChainParser :: Parser Opts
    initChainParser = subparser $ command "init" $
        info (helper <*> initChainOptsParser) (progDesc "Init new chain")
      where
        initChainOptsParser :: Parser Opts
        initChainOptsParser = do
          initAcc <- initAccParser
          chainOpts <- optsParser
          return chainOpts { _command = Driver.Chain (Init initAcc) }

        -- Used for spinning up nodes on the test network
        initAccParser :: Parser Account.AccountPrompt
        initAccParser = flag Account.Prompt Account.NoPrompt $
             long "new-account"
          <> help "Don't prompt for new account creation."

    optsParser :: Parser Opts
    optsParser = do
      _rpcPort   <- rpcPort
      _port      <- port
      _nonetwork <- nonetwork
      _chainConfig <- chainConfig
      _config    <- config
      _hostname  <- hostname
      _bootnodes <- bootnodes
      _storageBackend <- storageBackend
      _verbose   <- verbose
      _rpcReadOnly <- rpcReadOnly
      _testMode  <- testMode
      _privKey   <- privKey
      _nodeDir   <- nodeDir

      pure Opts {..}


    _compile     = Nothing
    _scriptInput = Nothing
    _command     = Driver.Chain Run

keyParser :: Parser Opts
keyParser = subparser $ command "authority"
    (info (helper <*> keyParser')
    (progDesc "Manage keypairs"))
  where
    keyParser' :: Parser Opts
    keyParser' = do
      n <- nAuths
      pure defaultOpts
        { _command = Driver.Keys (Opts.CreateAuthorities n)
        }

    nAuths :: Parser Int
    nAuths = argument auto (metavar "N")

-------------------------------------------------------------------------------
-- Data Commands
-------------------------------------------------------------------------------


dataParser :: Parser Opts
dataParser =
  dataGetParser
  <|> dataListParser
  <|> dataExport
  <|> dataCommitParser
  <|> dataLoadAssetParser
  <|> dataLoadAccountParser

dataGetParser :: Parser Opts
dataGetParser =  subparser $ command "get"
    (info (helper <*> dataParser')
    (progDesc "Get by address"))
  where
    dataParser' :: Parser Opts
    dataParser' = do
      addr <- addrParser
      pure defaultOpts
        { _command = Driver.Data (Get addr)
        }

dataListParser :: Parser Opts
dataListParser = subparser $ command "list"
    (info (helper <*> dataParser')
    (progDesc "List all"))
  where
    dataParser' :: Parser Opts
    dataParser' = pure defaultOpts { _command = Driver.Data List }

dataCommitParser :: Parser Opts
dataCommitParser = subparser $ command "commit"
    (info (helper <*> scriptParser')
    (progDesc "Commit local storage"))
  where
    scriptParser' :: Parser Opts
    scriptParser' = do
      compile <- fileParser
      contractAddr    <- addrParser
      accountAddr    <- addrParser
      pure defaultOpts
        { _command = Driver.Data (Commit compile contractAddr accountAddr)
        }

dataLoadAssetParser :: Parser Opts
dataLoadAssetParser = subparser $ command "loadAsset"
    (info (helper <*> scriptParser')
    (progDesc "Load Asset at Address into AssetDB"))
  where
    scriptParser' :: Parser Opts
    scriptParser' = do
      fp        <- fileParser
      assetAddr <- addrParser
      pure defaultOpts
        { _command = Driver.Data (LoadAsset fp assetAddr)
        }

dataLoadAccountParser :: Parser Opts
dataLoadAccountParser = subparser $ command "loadAccount"
    (info (helper <*> scriptParser')
    (progDesc "Load Account into AccountDB"))
  where
    scriptParser' :: Parser Opts
    scriptParser' = do
      fp        <- fileParser
      pure defaultOpts
        { _command = Driver.Data (LoadAccount fp)
        }

dataExport :: Parser Opts
dataExport = subparser $ command "export"
    (info (helper <*> scriptParser')
    (progDesc "Export ledger data to file"))
  where
    scriptParser' :: Parser Opts
    scriptParser' = do
      fp <- fileParser
      pure defaultOpts
        { _command = Driver.Data (Export fp)
        }

-------------------------------------------------------------------------------
-- Script Commands
-------------------------------------------------------------------------------

scriptParser :: Parser Opts
scriptParser =
  scriptCompileParser
  <|> scriptReplParser
  <|> scriptFormat
  <|> scriptLint
  <|> scriptGraph

scriptReplParser :: Parser Opts
scriptReplParser = subparser $ command "repl"
    (info (helper <*> repl)
    (progDesc "Compile, typecheck a script, and load it into a REPL."))
  where
    repl :: Parser Opts
    repl = do
      compile     <- optional fileParser
      verboseFlag <- verbose
      useJson     <- json
      pure defaultOpts
        { _command = Driver.Script (ReplScript compile verboseFlag)
        }

    json :: Parser Bool
    json = flag False True $
         long "json"
      <> short 'j'
      <> help "JSON output"

    verbose :: Parser Bool
    verbose = flag False True $
         long "verbose"
      <> short 'v'
      <> help "Verbose logging"

scriptFormat :: Parser Opts
scriptFormat = subparser $ command "format"
    (info (helper <*> scriptParser')
    (progDesc "Format a script"))
  where
    scriptParser' :: Parser Opts
    scriptParser' = do
      compile <- fileParser
      inplace' <- inplace
      pure defaultOpts
        { _command = Driver.Script (Format compile)
        }

    inplace :: Parser Bool
    inplace = flag False True $
         long "inplace"
      <> short 'i'
      <> help "Modify file in place"


scriptCompileParser :: Parser Opts
scriptCompileParser = subparser $ command "compile"
    (info (helper <*> scriptParser')
    (progDesc "Compile and typecheck a script."))
  where
    scriptParser' :: Parser Opts
    scriptParser' = do
      compile <- fileParser
      localStorage <- localStorage
      pure defaultOpts
        { _command = Driver.Script (CompileScript compile localStorage)
        }

    localStorage :: Parser (Maybe FilePath)
    localStorage = optional $ strOption $
         long "localStorage"
      <> short 's'
      <> metavar "PATH"
      <> help "JSON to check locals against"


scriptLint :: Parser Opts
scriptLint = subparser $ command "lint"
    (info (helper <*> scriptParser')
    (progDesc "Lint a script."))
  where
    scriptParser' :: Parser Opts
    scriptParser' = do
      compile <- fileParser
      pure defaultOpts
        { _command = Driver.Script (Lint compile)
        }


scriptGraph :: Parser Opts
scriptGraph = subparser $ command "graph"
    (info (helper <*> scriptParser')
    (progDesc "Extract graph from a script."))
  where
    scriptParser' :: Parser Opts
    scriptParser' = do
      compile <- fileParser
      pure defaultOpts
        { _command = Driver.Script (Graph compile)
        }


-------------------------------------------------------------------------------
  -- Console Command
-------------------------------------------------------------------------------
consoleParser :: Parser Opts
consoleParser = consoleParser'
  where
    consoleParser' :: Parser Opts
    consoleParser' = do
      _host <- hostname
      _port <- port
      pure defaultOpts { _port = _port, _hostname = _host, _command = Console }


-------------------------------------------------------------------------------
-- Parser Utils
-------------------------------------------------------------------------------

fileParser :: Parser [Char]
fileParser = strArgument (metavar "FILE")

addrParser :: Parser Address.Address
addrParser = argument (eitherReader addrParser') (metavar "ADDR")
  where
    addrParser' :: [Char] -> Either [Char] Address.Address
    addrParser' a = maybe (Left "Address given is invalid") Right (Address.parseAddr (BS.pack a))

port :: Parser (Maybe Int)
port = optional $ option auto $
     long "port"
  <> short 'p'
  <> metavar "PORT"
  <> help "Override the port on which the node should listen for P2P. Default 8001"

rpcPort :: Parser (Maybe Int)
rpcPort = optional $ option auto $
     long "rpc-port"
  <> metavar "PORT"
  <> help "Override the port on which the node should listen for RPC. Default: 8545"

config :: Parser (Maybe FilePath)
config = optional $ strOption $
     long "config"
  <> short 'c'
  <> metavar "PATH"
  <> help "Specify the node configuration file"

chainConfig :: Parser (Maybe FilePath)
chainConfig = optional $ strOption $
     long "chain-config"
  <> short 'g'
  <> metavar "PATH"
  <> help "Specify the chain (genesis block & consensus) configuration to initialize chain"

storageBackend :: Parser (Maybe [Char])
storageBackend = optional $ strOption $
     long "backend"
  <> short 'b'
  <> metavar "URI"
  <> help "Specify the storage backend uri"

genesis :: Parser (Maybe FilePath)
genesis = optional $ strOption $
     long "genesis"
  <> short 'g'
  <> metavar "PATH"
  <> help "Specify the genesis block configuration to initialize chain"

nonetwork :: Parser (Maybe Bool)
nonetwork = optional $ flag False True $
     long "no-network"
  <> help "Disable p2p networking."

hostname :: Parser (Maybe [Char])
hostname = optional $ strOption $
     long "hostname"
  <> short 'h'
  <> metavar "HOST"
  <> help "Specify hostname"

bootnodes :: Parser (Maybe [ByteString])
bootnodes = optional $ fmap (map BS.pack . words) <$> strOption $
     long "bootnodes"
  <> help "Specify boot nodes"

verbose :: Parser (Maybe Bool)
verbose = optional $ flag False True $
     long "verbose"
  <> short 'v'
  <> help "Verbose logging"

rpcReadOnly :: Parser (Maybe Bool)
rpcReadOnly = optional $ flag False True $
     long "rpc-read-only"
  <> help "RPC read only"

testMode :: Parser (Maybe Bool)
testMode = optional $ flag False True $
     long "test"
  <> help "Run node in test mode"

privKey :: Parser (Maybe FilePath)
privKey = optional $ strOption $
     long "privkey"
  <> short 'k'
  <> metavar "PATH"
  <> help "Private key"

nodeDir :: Parser (Maybe FilePath)
nodeDir = optional $ strOption $
     long "datadir"
  <> short 'd'
  <> metavar "PATH"
  <> help "Specify the node data directory path"

-------------------------------------------------------------------------------
-- Toplevel
-------------------------------------------------------------------------------

versionCmd :: Parser Opts
versionCmd = pure defaultOpts { _command = Version }

copts :: Parser Opts
copts = subparser
  (
      command "chain"    (info (helper <*> chainParser)  (progDesc "Manage and setup blockchains."))
   <> command "keys"     (info (helper <*> keyParser)    (progDesc "Manage keypairs."))
   <> command "scripts"  (info (helper <*> scriptParser) (progDesc "Manage smart contract scripts."))
   <> command "data"     (info (helper <*> dataParser)   (progDesc "Manage ledger data and oracle services."))
   <> command "console"  (info (helper <*> consoleParser)(progDesc "Connect to a node and issue transactions."))
   <> command "version"  (info (helper <*> versionCmd)   (progDesc "Version info"))
  )

handleOpts :: Opts -> IO ()
handleOpts o@Opts {..} = do
  hSetBuffering stdout NoBuffering

  let
    silent = case Opts._command o of
      Chain _  -> False
      Keys _   -> True
      Script _ -> True
      Data _   -> True
      Console  -> False
      Version  -> False

  case Opts._command o of
      Chain _  -> putStrLn banner
      Keys _   -> pure ()
      Script _ -> pure ()
      Data _   -> pure ()
      Console  -> putStrLn banner
      Version  -> putStrLn banner

  -- Load Config from command line fallbacking to 'defaultConfig'
  c@Config.Config {}
    <- Config.handleConfig silent (maybe Config.defaultConfig identity _config)

  backend <- mapM Config.getStorageBackend _storageBackend
  -- Load config falling back to defaults.
  let config @ Config.Config {..}
        = Config.toggleVerbosity _verbose
          $ c
            { Config.configFile
                = _config      `fallback` Config.defaultConfig
            , Config.nonetwork
              = _nonetwork   `fallback` (Config.nonetwork c)
            , Config.port
              = _port        `fallback` (Config.port c)
            , Config.rpcPort
              = _rpcPort     `fallback` (Config.rpcPort c)
            , Config.chainConfigFile
              = _chainConfig   `fallback` (Config.chainConfigFile c)
            , Config.hostname
              = _hostname    `fallback` (Config.hostname c)
            , Config.bootnodes
              = fromMaybe [] _bootnodes ++ Config.bootnodes c
            , Config.storageBackend
              = backend   `fallback` (Config.storageBackend c)
            , Config.rpcReadOnly
              = _rpcReadOnly `fallback` (Config.rpcReadOnly c)
            , Config.testMode
              = _testMode    `fallback` (Config.testMode c)
            , Config.nodeDataDir
              = _nodeDir     `fallback` (Config.nodeDataDir c)
            }

  Driver.driver (Opts._command o) o config

  where
    fallback :: Maybe a -> a -> a
    fallback = flip fromMaybe

-- Command options
coptions :: ParserInfo Opts
coptions = info (helper <*> copts) idm

-------------------------------------------------------------------------------
-- Entry
-------------------------------------------------------------------------------

cprefs :: ParserPrefs
cprefs = prefs showHelpOnError

-- Parse commmand line flags and handle application
main :: IO ()
main = customExecParser cprefs coptions >>= handleOpts
