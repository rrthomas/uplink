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
    initChainParser =
        subparser $ command "init" $
          info (helper <*> initChainOptsParser) $
            progDesc "Initialize a new Uplink node and database."
      where
        initChainOptsParser :: Parser Opts
        initChainOptsParser = do
          init <- Init <$> initAccParser <*> importDataParser
          chainOpts <- optsParser
          return chainOpts { _command = Driver.Chain init }

        -- Used for spinning up nodes on the test network
        initAccParser :: Parser Account.AccountPrompt
        initAccParser = flag Account.Prompt Account.NoPrompt $
             long "new-account"
          <> help "Don't prompt for new account creation."

        -- Load blocks or world state from a file
        importDataParser :: Parser (Maybe ImportData)
        importDataParser = optional $
            importBlocksParser <|> importLedgerParser
          where
            -- Load blocks from a file
            importBlocksParser :: Parser ImportData
            importBlocksParser =
              fmap ImportBlocks $
                strOption $
                     long "import-blocks"
                  <> metavar "FILE"
                  <> help "Import blocks from a file storing blocks as XML or JSON."

            -- Load ledger state from a file
            importLedgerParser :: Parser ImportData
            importLedgerParser =
              fmap ImportLedger $
                strOption $
                     long "import-ledger"
                  <> metavar "FILE"
                  <> help "Import ledger from a file storing ledger data as JSON."

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
dataParser = do
  cmd <- dataGetParser
     <|> dataListParser
     <|> dataExport
     <|> dataCommitParser
     <|> dataLoadAssetParser
     <|> dataLoadAccountParser
  db <- storageBackend
  pure $ cmd { _storageBackend = db }

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
dataExport =
  subparser $ command "export" $
    info (helper <*> dataExportCmdParser) $
      progDesc "Export data to file"
  where
    dataExportCmdParser :: Parser Opts
    dataExportCmdParser = do
      export <- Export
            <$> exportDataParser
            <*> fileParser
      pure defaultOpts
        { _command = Driver.Data export
        }

    exportDataParser = exportLedgerJSON <|> exportBlocks

    exportLedgerJSON :: Parser ExportData
    exportLedgerJSON =
      subparser $ command "ledger" $
        info (helper <*> pure ExportLedgerState) $
          progDesc "Export ledger data as JSON to a file."

    exportBlocks :: Parser ExportData
    exportBlocks =
      subparser $ command "blocks" $
        info (helper <*> fmap ExportBlocks formatParser) $
          progDesc "Export blocks to a file in a given format"

-------------------------------------------------------------------------------
-- Script Commands
-------------------------------------------------------------------------------

scriptParser :: Parser Opts
scriptParser =
  scriptCompileParser
  <|> scriptFormat
  <|> scriptLint
  <|> scriptGraph

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
  -- Repl Command
-------------------------------------------------------------------------------
replParser :: Parser Opts
replParser = replParser'
  where
    replParser' :: Parser Opts
    replParser' = do
      _host <- hostname
      _port <- port
      _script <- fileParser
      _verbose <- verbose

      _worldState <- optional fileParser
      pure defaultOpts { _port = _port, _hostname = _host, _command = (Repl _script (fromMaybe False _verbose) _worldState)  }


-------------------------------------------------------------------------------
-- Parser Utils
-------------------------------------------------------------------------------

fileParser :: Parser [Char]
fileParser = strArgument (metavar "FILE")

addrParser :: Parser (Address.Address a)
addrParser = argument (eitherReader addrParser') (metavar "ADDR")
  where
    addrParser' :: [Char] -> Either [Char] (Address.Address a)
    addrParser' a =
      first toS (Address.parseAddress (BS.pack a))

formatParser :: Parser DataFormat
formatParser =
  fmap (fromMaybe JSON) $
    optional $ option auto $
      mconcat [ long "format"
              , short 'f'
              , help "Format of data output"
              ]

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
   <> command "repl"     (info (helper <*> replParser)   (progDesc "Connect to a node and simulate a contract."))
   <> command "version"  (info (helper <*> versionCmd)   (progDesc "Version info"))
  )

handleOpts :: Opts -> IO ()
handleOpts o@Opts {..} = do
  hSetBuffering stdout NoBuffering

  let
    silent = case Opts._command o of
      Chain _    -> False
      Keys _     -> True
      Script _   -> True
      Data _     -> True
      Console    -> False
      Repl _ _ _ -> False
      Version    -> False

  case Opts._command o of
      Chain _    -> putStrLn banner
      Keys _     -> pure ()
      Script _   -> pure ()
      Data _     -> pure ()
      Console    -> putStrLn banner
      Repl _ _ _ -> putStrLn banner
      Version    -> putStrLn banner

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
