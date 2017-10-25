{-# LANGUAGE StrictData #-}

{-|

Command line options and commands.

-}

module Opts (
  -- ** Options
  Opts(..),
  defaultOpts,

  -- ** Commands
  Command(..),

  KeyCommand(..),
  DataCommand(..),
  ChainCommand(..),
  ScriptCommand(..),
) where

import Protolude
import qualified Config
import qualified Account
import qualified Address

-------------------------------------------------------------------------------
-- Command Line Interface
-------------------------------------------------------------------------------

data Command
  = Chain   ChainCommand
  | Console
  | Keys    KeyCommand
  | Script  ScriptCommand
  | Data    DataCommand
  | Version
  deriving (Eq, Ord, Show)

data KeyCommand
  = CreateAuthorities { nAuths :: Int }
  deriving (Eq, Ord, Show)

data ScriptCommand
  = CompileScript { file :: FilePath, localStorage :: Maybe FilePath }
  | ReplScript { script :: Maybe FilePath, verbose :: Bool }
  | Lint { file :: FilePath }
  | Format { file :: FilePath }
  | Graph { file :: FilePath }
  deriving (Eq, Ord, Show)

data DataCommand
  = Get Address.Address
  | List
  | Commit
    { fpath        :: FilePath
    , contractAddr :: Address.Address
    , accountAddr  :: Address.Address
    }
  | Export { fPath :: FilePath }
  | LoadAsset FilePath Address.Address
  | LoadAccount FilePath
  deriving (Eq, Ord, Show)

data ChainCommand
  = Run
  | Init Account.AccountPrompt
  deriving (Eq, Ord, Show)

-- Overloaded configuration settings passed on the commandline that supercede
-- configuration files.

-- | Command line options.
data Opts = Opts
  { _rpcPort     :: Maybe Int
  , _port        :: Maybe Int
  , _dbpath      :: Maybe FilePath
  , _nonetwork   :: Maybe Bool
  , _chainConfig :: Maybe FilePath
  , _config      :: Maybe FilePath
  , _verbose     :: Maybe Bool
  , _hostname    :: Maybe [Char]
  , _bootnodes   :: Maybe [ByteString]
  , _command     :: Command
  , _rpcReadOnly :: Maybe Bool
  , _testMode    :: Maybe Bool
  , _privKey     :: Maybe FilePath
  } deriving (Show)

defaultOpts :: Opts
defaultOpts = Opts
  { _rpcPort     = Just 8545
  , _port        = Just 8001
  , _dbpath      = Just ".uplink"
  , _nonetwork   = Just False
  , _chainConfig = Just "chain.config"
  , _config      = Just Config.defaultConfig
  , _verbose     = Just False
  , _hostname    = Nothing
  , _bootnodes   = Nothing
  , _command     = Chain Run
  , _rpcReadOnly = Just False
  , _testMode    = Just False
  , _privKey     = Nothing
  }
