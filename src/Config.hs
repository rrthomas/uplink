{-|

There are three different configurations that are avaiable on a node.

* Account settings ( private key, public key, metadata )
  - key.pem
  - key.pub
* Chain settings ( genesis block, concensus mechanism )
  - chain.config
* Node settings ( port, rpc settings, data directory, network settings )
  - node.config

-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

module Config (
  -- ** Types
  StorageBackend(..),
  Config(..),
  readConfig,
  handleConfig,
  verifyConfig,
  defaultConfig,
  getStorageBackend,

  -- ** Errors
  errorHandler,

  -- ** Chain Settings
  ChainSettings(..),
  readChain,
  handleChain,
  defaultChain,

  -- * Logging settings
  toggleVerbosity,

  -- * Transport Settings
  Transport(..),
  TLSConfig(..),

  -- ** Testing
  testChain,
  testConfig,
) where

import Protolude

import Data.List (elemIndex)
import qualified Data.Configurator.Types as C

import qualified Utils
import qualified Network.Utils as NU
import qualified Network.P2P.Logging as Log
import qualified Data.Configurator as C
import qualified Consensus.Authority.Params as CAP

import DB
import Database.PostgreSQL.Simple

import Network.URI (URIAuth, parseURI, uriScheme, uriPath, uriAuthority, uriUserInfo, uriPort, uriRegName)
import Network.BSD (getHostName)

import System.FilePath
import System.Directory
import System.Posix.Files

-- | Node configuration, all arguments are required and handled by override and
-- defaulting logic in `Main`.
data Config = Config
  { loggingRules :: [Log.LogRule] -- ^ Logging configuration
  , bootnodes    :: [ByteString]  -- ^ Bootnodes
  , rpcPort      :: Int           -- ^ RPC port
  , rpcSsl       :: Bool          -- ^ Use TLS for RPC
  , rpcCrt       :: FilePath      -- ^ RPC TLS certificate
  , rpcKey       :: FilePath      -- ^ RPC TLS key
  , port         :: Int           -- ^ Port
  , hostname     :: [Char]        -- ^ Hostname
  , maxPeers     :: Int           -- ^ Maximum peer count
  , minPeers     :: Int           -- ^ Minimum peer count
  , closed       :: Bool          -- ^ Closed network
  , storageBackend :: StorageBackend -- ^ Type of Database to be used
  , nonetwork    :: Bool          -- ^ Disable networking
  , configFile   :: FilePath      -- ^ Config file
  , chainConfigFile :: FilePath   -- ^ Chain configuration file
  , nodeDataDir  :: FilePath      -- ^ Directory where local files node data files (keys, etc) are stored
  , rpcReadOnly  :: Bool          -- ^ Can RPC cmds change state of ledger
  , preallocated :: FilePath      -- ^ Pre-allocated accounts directory
  , testMode     :: Bool          -- ^ Is node in "test" mode
  , accessToken  :: FilePath      -- ^ Filepath to network-access-token
  , transport    :: Transport     -- ^ Transport used for intra-node communication
  , monitorPort  :: Int           -- ^ Port to run monitoring server on
  } deriving (Show)

data ChainSettings = ChainSettings
  { genesisTimestamp :: Int64      -- ^ Genesis block timestamp
  , genesisHash      :: ByteString -- ^ Genesis block hash
  , genesisPoA       :: CAP.PoA    -- ^ PoA Cosensus Algorithm Parameters
  } deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

getDefault :: [Char] -> IO FilePath
getDefault "XDG_DATA_HOME"   = userRelative $ ".local" </> "share"
getDefault "XDG_CONFIG_HOME" = userRelative $ ".config"
getDefault "XDG_CACHE_HOME"  = userRelative $ ".cache"
getDefault "XDG_DATA_DIRS"   = return $ "/usr/local/share:/usr/share"
getDefault "XDG_CONFIG_DIRS" = return $ "/etc/xdg"
getDefault _                 = return $ ""

userRelative :: FilePath -> IO FilePath
userRelative p = getHomeDirectory >>= return . (</> p)

macOsPath :: FilePath -> IO FilePath
macOsPath cfg = userRelative $ "/Library/Application Support/" </> "uplink" </> "config" </> cfg

fallbacks :: FilePath -> IO [FilePath]
fallbacks cfg = sequence [
  -- Linux
    pure cfg                                    -- ./node.config
  , pure ("/etc/uplink" </> cfg)                -- /etc/uplink/node.config
  , (</> "uplink" </> cfg) <$> getDefault "XDG_CONFIG_HOME"  -- '$HOME/.config/uplink/node.config'

  -- MacOS
  , macOsPath cfg                               -- '$HOME/Library/Application Support/uplink/node.config'
  ]

-- | Fallback paths for finding configuration files
configFallback :: Bool -> [FilePath] -> IO (Maybe FilePath)
configFallback silent [] = pure Nothing
configFallback silent (x:xs) = do
  when (not silent) $
    putText $ "Searching for config at: " <> toS x
  cfgExists <- fileExist x
  if cfgExists
    then pure (Just x)
    else configFallback silent xs

-- | Find a configuration file somewhere in the users filesystem
handleConfig :: Bool -> FilePath -> IO Config
handleConfig silent cfgFile = do
  cfgs <- fallbacks (takeFileName cfgFile)
  cfg  <- configFallback silent (cfgFile : cfgs)
  case cfg of
    Nothing -> die $ "Cannot find configuration: " <> ((toS cfgFile) :: Text)
    Just fpath -> readConfig silent fpath

-- | Read configuration file from a directory
readConfig :: Bool -> FilePath -> IO Config
readConfig silent cfgFile = errorHandler $ do
  unless silent $
    Utils.putGreen ("Using configuration: " <> toS cfgFile)

  cfg          <- C.load [C.Required cfgFile]
  loggingRules <- C.require cfg "logging.rules"
  backend      <- getStorageBackend =<<
                    C.require cfg "storage.backend"
  nodeDataDir  <- C.require cfg "storage.directory"

  rpcPort      <- C.require cfg "rpc.port"
  rpcSsl       <- C.require cfg "rpc.ssl"
  rpcCrt       <- C.require cfg "rpc.crt"
  rpcKey       <- C.require cfg "rpc.key"

  port         <- C.require cfg "network.port"
  hostname     <- getHostname port =<<
                    C.lookup cfg "network.hostname"
  nonetwork    <- C.require cfg "network.nonetwork"
  bootnodes    <- C.require cfg "network.bootnodes"
  closed       <- C.require cfg "network.closed"
  minPeers     <- C.require cfg "network.min-peers"
  maxPeers     <- C.require cfg "network.max-peers"

  preallocated <- C.require cfg "network.preallocated"
  accessToken  <- C.require cfg "network.access-token"

  -- Parse the mode of transport (InMemory, TCP, TLS)
  transport    <- parseTransport cfg

  monitorPort  <- C.lookup cfg "network.monitor-port"

  case Log.verifyRules loggingRules of
    Left err
      -> pure $ Left err
    Right rules
      -> pure . Right $ Config
         { loggingRules = rules
         , bootnodes    = bootnodes
         , rpcPort      = rpcPort
         , rpcSsl       = rpcSsl
         , rpcCrt       = rpcCrt
         , rpcKey       = rpcKey
         , port         = port
         , hostname     = hostname
         , minPeers     = minPeers
         , maxPeers     = maxPeers
         , closed       = closed
         , storageBackend = backend
         , nonetwork    = nonetwork
         , configFile   = cfgFile
         , chainConfigFile = defaultChain
         , nodeDataDir  = nodeDataDir
         , rpcReadOnly  = False
         , preallocated = preallocated
         , testMode     = False
         , accessToken  = accessToken
         , transport    = transport
         , monitorPort  = fromMaybe 0 monitorPort
         }

getHostname :: Int -> Maybe [Char] -> IO [Char]
getHostname port mHostname = do
  host <-
    case mHostname of
      Nothing -> getHostName
      Just hn -> pure hn
  errorHandler $
    fmap (fmap fst) $
      NU.resolve (host, show port)

getStorageBackend :: [Char] -> IO StorageBackend
getStorageBackend cs =
  case parseURI cs of
    Nothing -> die $ "Invalid storage backend uri: " <> toS cs
    (Just uri) -> do
      let scheme   = uriScheme uri
          path = drop 1 $ uriPath uri

      case scheme of
        "leveldb:" -> return $ LevelDB $ if null path then ".uplink" else path
        "postgresql:" -> case uriAuthority uri of
            Nothing -> die $ "invalid uri auth given: " <> toS cs

            (Just as) -> do
              let host = if null (uriRegName as)
                            then Nothing
                            else Just $ uriRegName as

              let port = if null (uriPort as)
                            then Nothing
                            else (readMaybe (drop 1 $ uriPort as) :: Maybe Word16)

              let (user, password) = parseAuthInfo as

              return $ PostgreSQL ConnectInfo {
                connectHost = host `fallback` connectHost defaultConnectInfo
              , connectPort = port `fallback` connectPort defaultConnectInfo
              , connectUser = user `fallback` connectUser defaultConnectInfo
              , connectPassword = password `fallback` connectPassword defaultConnectInfo
              , connectDatabase = if null path then "uplink" else path
              }
        _ -> die $ "Invalid uri scheme given: " <> toS cs
  where
    parseAuthInfo :: URIAuth -> (Maybe [Char], Maybe [Char])
    parseAuthInfo as =
      case elemIndex ':' $ uriUserInfo as of
        Nothing ->
          let user = dropLast $ uriUserInfo as in
            if null user then (Nothing, Nothing) else (Just user, Nothing)
        (Just i) ->
          case splitAt i $ uriUserInfo as of
            (user, ':' : password) -> (Just user, Just $ dropLast password)
            _ -> (Nothing, Nothing)

    fallback = flip fromMaybe

    dropLast "" = ""
    dropLast [_] = ""
    dropLast (x : xs) = x : dropLast xs

-------------------------------------------------------------------------------
-- Chain Configuration
-------------------------------------------------------------------------------

-- | Find a configuration file somewhere in the users filesystem
handleChain :: Bool -> FilePath -> IO ChainSettings
handleChain silent cfgFile = do
  cfgs <- fallbacks (takeFileName cfgFile)
  cfg  <- configFallback silent (cfgFile : cfgs)
  case cfg of
    Nothing -> die $ "Cannot find chain configuration: " <> ((toS cfgFile) :: Text)
    Just fpath -> readChain silent fpath

readChain :: Bool -> FilePath -> IO ChainSettings
readChain silent cfgFile = do
  exists <- fileExist cfgFile
  if (not exists)
    then die $ "Chain configuration file does not exist at: " <> (show cfgFile)
    else pure ()

  cfg           <- C.load [C.Required cfgFile]

  -- PoA consensus params
  validators    <- C.require cfg "authority.validators"
  blockPeriod   <- C.require cfg "authority.block-period"
  blockGenLimit <- C.require cfg "authority.block-gen-limit"
  blockSignLimit <- C.require cfg "authority.block-sign-limit"
  threshold     <- C.require cfg "authority.threshold"
  minTxs        <- C.require cfg "authority.transactions"

  let eValidPoAParams =
        CAP.mkGenesisPoA
          validators
          (blockPeriod * 1000000) -- turn into microseconds
          blockGenLimit
          blockSignLimit
          threshold
          minTxs
  poaParams <- case eValidPoAParams of
    Left err -> die $ "PoA Consensus parameters invalid:\n  " <> show err
    Right poa -> pure poa

  -- Genesis block params
  timestamp     <- C.require cfg "genesis.timestamp"
  genesisHash   <- C.require cfg "genesis.hash"

  return ChainSettings {
    genesisTimestamp  = timestamp
  , genesisHash       = genesisHash
  , genesisPoA        = poaParams
  }

-- | Verify the integrity of a configuration set.
verifyConfig :: Config -> IO Bool
verifyConfig Config {..} = do
  exists <- mapM doesFileExist [
      configFile
    ]
  let
    ports = [portValid rpcPort, portValid port]
    conds = [
        not (null bootnodes)
      , not (null hostname)
      , (>= 0) minPeers
      , (>= -1) maxPeers
      , minPeers < maxPeers
      ]
  {-doesDirectoryExist dbpath-}
  pure (and $ exists <> ports <> conds)

portValid :: Int -> Bool
portValid x = x > 1023 && x < 65535

nonZero :: Integral a => a -> Bool
nonZero = (>0)

-- | Default configuration file path.
defaultConfig :: FilePath
defaultConfig = "config/node.config"

-- | Default configuration for chain.
defaultChain :: FilePath
defaultChain = "config/chain.config"

errorHandler :: IO (Either Text a) -> IO a
errorHandler m = do
  res <- try m
  case res of
    Left (C.KeyError nm) -> Utils.dieRed $ "Configuration is missing value: " <> nm
    Right (Left err)   -> Utils.dieRed $ "Configuration is invalid: " <> err
    Right (Right val)  -> pure val

-------------------------------------------------------------------------------
-- Logging settings
-------------------------------------------------------------------------------

toggleVerbosity :: Maybe Bool -> Config -> Config
toggleVerbosity maybeVerbosity c
  = c { loggingRules = Log.toggleVerbosity wantVerbosity (loggingRules c) }
    where
      wantVerbosity = fromMaybe False maybeVerbosity

-------------------------------------------------------------------------------
-- TLS settings
-------------------------------------------------------------------------------

-- | Datatype specifying which mode the network operates in
data Transport
  = InMemory          -- ^ Used for testing a single node (cannot use intra-transport communication)
  | TCP               -- ^ Reliable TCP connections between uplink nodes
  | TLS TLSConfig -- ^ Reliable TLS connections (using TCP) between uplink nodes
  deriving (Show)

data TLSConfig = TLSConfig
  { serverCertFp :: FilePath       -- ^ Filepath of Server TLS Certificate
  , serverKeyFp  :: FilePath       -- ^ Filepath of Server Private Key
  , mCertStoreFp :: Maybe FilePath -- ^ Filepath to certificate store
  } deriving (Show)

parseTransport :: C.Config -> IO Transport
parseTransport c = do
  mtransport <- C.lookup c "network.transport"
  case mtransport of
    Nothing -> pure TCP
    Just t -> case t of
      "in-memory" -> pure InMemory
      "tcp"       -> pure TCP
      "tls"       -> TLS <$> parseTLSConfig c
      other       -> throwIO $ C.KeyError ("Unsupported network.transport specified: " <> other)

-- | Parses the TLS config and then makes a valid TLSConfig value from the TLS
-- transport library. *A certificate store could potentially be supplied when
-- implementing the network-access-token on the transport layer.
parseTLSConfig :: C.Config -> IO TLSConfig
parseTLSConfig c = do
  serverCertFp  <- C.require c "network.tls.server-certificate"
  serverKeyFp   <- C.require c "network.tls.server-key"
  mCertStoreFp  <- C.lookup c "network.tls.certificate-store"
  pure $ TLSConfig serverCertFp serverKeyFp mCertStoreFp

-------------------------------------------------------------------------------
-- Testing
-------------------------------------------------------------------------------

testConfig :: IO Config
testConfig = do
  cfg <- C.load [C.Required defaultConfig]
  C.display cfg
  readConfig False defaultConfig

testChain :: IO ChainSettings
testChain = do
  cfg <- C.load [C.Required defaultChain]
  chain <- readChain True defaultChain
  Utils.ppDump chain
  return chain
