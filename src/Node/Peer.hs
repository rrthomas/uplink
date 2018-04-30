{- |

Uplink Node Peers

-}

{-# LANGUAGE DeriveGeneric #-}

module Node.Peer (
  Peer(..),
  Peers,

  peersToPids,
  peersToNodeIds,
  peersToAddresses,
  pidToNodeIdBS,
  nodeIdToHostname,

  peersFile,
  readPeers,
  readPeers',
  writePeers,
  writePeers',

) where

import Protolude

import Control.Distributed.Process
import Network.Transport (endPointAddressToByteString)

import Data.Aeson
import qualified Data.Binary as Binary
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as BSL

import Address
import Utils

import System.FilePath

-------------------------------------------------------------------------------
-- Node Peers
-------------------------------------------------------------------------------

data Peer = Peer
  { peerPid     :: ProcessId  -- ProcessId of Peer Controller Process
  , peerAccAddr :: (Address AAccount)    -- Address of Peer node account
  } deriving (Eq, Ord, Show, Generic)

instance Binary.Binary Peer where
  put (Peer pid addr) = do
    Binary.put pid
    Binary.put addr
  get = Peer <$> Binary.get <*> Binary.get

instance ToJSON Peer where
  toJSON (Peer pid addr) = object
    [ "tag" .= ("Peer" :: Text)
    , "contents" .= object
        [ "peerPid" .= decodeUtf8 (pidToNodeIdBS pid)
        , "peerAccAddr" .= addr
        ]
    ]

type Peers = Set.Set Peer

peersToPids :: Peers -> [ProcessId]
peersToPids = map peerPid . Set.toList

peersToNodeIds :: Peers -> [NodeId]
peersToNodeIds = map (processNodeId . peerPid) . Set.toList

peersToAddresses :: Peers -> [Address AAccount]
peersToAddresses = map peerAccAddr . Set.toList

pidToNodeIdBS :: ProcessId -> ByteString
pidToNodeIdBS = endPointAddressToByteString . nodeAddress . processNodeId

nodeIdToHostname :: NodeId -> ByteString
nodeIdToHostname = toS . takeWhile (/= ':') . toS . endPointAddressToByteString . nodeAddress

-------------------------------------------------------------------------------
-- Peers Persistence
-------------------------------------------------------------------------------

peersFile :: FilePath -> FilePath
peersFile root = root </> "peers"

-- | Read a file containing the binary encoding
-- of a node's peers of the form <root>/peers
readPeers :: FilePath -> IO (Either Text Peers)
readPeers root = do
   let peersFile' = peersFile root
   fmap Binary.decode <$> safeReadLazy peersFile'

-- | Read a file containing the binary encoding
-- of a node's peers at the given filepath
readPeers' :: FilePath -> IO (Either Text Peers)
readPeers' peersFile = do
  ebs <- safeReadLazy peersFile
  case ebs of
    Left err -> pure $ Left err
    Right "" -> pure $ Right $ Set.empty
    Right bs -> do -- V necessary otherwise throws with `error` :(
      let ePeers = Binary.decodeOrFail bs
      case ePeers of
        Left (_,_,err) -> pure $ Left $ toS err
        Right (_,_,peers) -> pure $ Right peers

-- | Write a node's binary encoded peers to a file of the form <root>/peers
writePeers :: FilePath -> Peers -> IO ()
writePeers root peers = do
   let peersFile' = peersFile root
   BSL.writeFile peersFile' $ Binary.encode peers

-- | Write a node's binary encoded peers to a file of the form <root>/peers
writePeers' :: FilePath -> Peers -> IO ()
writePeers' filepath peers = do
   BSL.writeFile filepath $ Binary.encode peers
