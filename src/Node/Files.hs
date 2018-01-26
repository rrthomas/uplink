{- |

Node data stored on the File system.

-}

module Node.Files (

  NodeData(..),
  NodeDataFilePaths(..),
  mkNodeDataFilePaths,

  initNodeDataFiles,
  initNodeDataDir,

  readNodeDataFiles,
  readNodeDataDir,

) where

import Protolude

import Node.Peer (Peers, peersFile, readPeers')
import Key       (ECDSAKeyPair)
import TxLog     (txLogFile)
import Utils

import Account   ( Account, readAccount, writeAccount_
                 , readKeys', writePubKey_, writePrivKey_
                 , accountFile, privKeyFile, pubKeyFile)

import System.Directory

-------------------------------------------------------------------------------
--
-- ** Node Data Persistance to FileSystem **
--
--   * [Node Account]
--     The account used by the uplink node to sign txs or create/sign blocks.
--     * Node account (JSON serialized)
--
--   * [Node Private Key]
--     Node private key from which the node account is derived
--     * Node account private key (PEM)
--
--   * [Node Public Key]
--     Node public key derived from the private key. Network public identifier.
--     * Node account public key (PEM)
--
--   * [Network Peers]
--     Serialization of all known peer process ids and their respective accound
--     addresses.
--
--   * [TxLog]
--     A list of modifications to world state resulting from smart contract
--     function calls, i.e. TxCall transactions.
--
-- ----------------------------------------------------------------------------
--
-- Regardless of the backend selected,
-- the Node will store files on disk:
--   ./<nodeDataDir>
--     - account       # JSON serialized account
--     - key           # PEM serialized private key
--     - key.pub       # PEM serialized public key
--     - peers         # Cached peer nodes
--     - txlog         # Log of ledger smart contract ledger updates
--
-------------------------------------------------------------------------------

data NodeDataFilePaths = NodeDataFilePaths
  { accountFile :: FilePath
  , privKeyFile :: FilePath
  , pubKeyFile  :: FilePath
  , txLogFile   :: FilePath
  , peersFile   :: FilePath
  }

data NodeData = NodeData
  { nodeAccount :: Account
  , nodeKeys    :: ECDSAKeyPair
  , nodePeers   :: Peers
  }

-- | Append the file names to the node data directory
mkNodeDataFilePaths :: FilePath -> NodeDataFilePaths
mkNodeDataFilePaths dir =
  NodeDataFilePaths
    (Account.accountFile dir)
    (Account.privKeyFile dir)
    (Account.pubKeyFile dir)
    (TxLog.txLogFile dir)
    (Node.Peer.peersFile dir)

-- | Initialize a directory to store node data files, and write relevant data to disk
-- Note: This file assumes the node data files do not exist, and will "fail" if they do
initNodeDataFiles
  :: NodeDataFilePaths -- ^ Filepaths corresponding to node data files
  -> Account           -- ^ Node account
  -> ECDSAKeyPair      -- ^ Node account keys
  -> IO (Either Text ())
initNodeDataFiles NodeDataFilePaths{..} account keys = do
    eAccFile     <- writeFileE writeAccFile     accountFile
    ePrivKeyFile <- writeFileE writePrivKeyFile privKeyFile
    ePubKeyFile  <- writeFileE writePubKeyFile  pubKeyFile
    eTxLogFile   <- writeFileE writeEmptyFile   txLogFile
    ePeersFile   <- writeFileE writeEmptyFile   peersFile
    pure $ sequence_ [eAccFile, ePrivKeyFile, ePubKeyFile, eTxLogFile, ePeersFile]
  where
    writeEmptyFile   = flip Utils.safeWrite mempty

    writeAccFile     = flip writeAccount_ account
    writePrivKeyFile = flip writePrivKey_ (snd keys)
    writePubKeyFile  = flip writePubKey_ (fst keys)

    -- Write node file data, failing if file exists
    writeFileE writeFile' filename = do
      fileExists <- doesFileExist filename
      if fileExists
        then pure $ Left $ "Failed to write Node data file: " <> show filename
        else writeFile' filename

initNodeDataDir
  :: FilePath
  -> Account
  -> ECDSAKeyPair
  -> IO (Either Text NodeDataFilePaths)
initNodeDataDir dir account keys = do
    dirExists <- doesDirectoryExist dir
    if dirExists
      then pure $ Left $
        "Node data directory '" <> toS dir <> "' already exists."
      else do
        createDirectoryIfMissing True dir
        eRes <- initNodeDataFiles nodeDataFilePaths account keys
        case eRes of
          Left err -> pure $ Left err
          Right _  -> pure $ Right nodeDataFilePaths
  where
    nodeDataFilePaths = mkNodeDataFilePaths dir

readNodeDataDir :: FilePath -> IO (Either Text NodeData)
readNodeDataDir = readNodeDataFiles . mkNodeDataFilePaths

-- | Read the Node data files from given the filepaths
readNodeDataFiles :: NodeDataFilePaths -> IO (Either Text NodeData)
readNodeDataFiles NodeDataFilePaths{..} = do
  eAccount <- Account.readAccount accountFile
  case eAccount of
    Left err  -> pure $ Left err
    Right acc -> do
      eKeys <- Account.readKeys' privKeyFile
      case eKeys of
        Left err   -> pure $ Left err
        Right keys -> do
          ePeers <- Node.Peer.readPeers' peersFile
          case ePeers of
            Left err    -> pure $ Left err
            Right peers -> pure $ Right $ NodeData acc keys peers
