{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE CPP #-}

module Network.Utils (
  mkNodeId,
  extractNodeId,
  resolveHostname,
) where

import Protolude

import Control.Distributed.Process
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as List

#ifndef OS_Mac
import Network.BSD (getHostByName, hostAddress)
import Network.Socket (inet_ntoa)
#endif

import Network.BSD (HostName)
import Network.Transport (EndPointAddress(..))

extractNodeId :: Process ByteString
extractNodeId = do
  -- nodeId is of "nid://ip:port:x"
  nodeId <- liftA show getSelfNode :: Process ByteString
  let len = length ("nid://" :: [Char])
  let nodeId' = BSC.drop len nodeId
  -- ip = "ip", rest = ":port:x"
  let (ip, rest) = BSC.break (==':') nodeId'
  -- port = "port", _ = ":x"
  let (port, _)  = BSC.break (==':') $ BSC.tail rest

  return $ ip <> ":" <> port

-- | Make a NodeId from "host:port" string.
mkNodeId :: ByteString -> IO NodeId
mkNodeId addr = do
    hostname <- resolveHostname host
    let epAddr = BSC.pack $ intercalate ":" [hostname, port, "0"]
    return $ NodeId $ EndPointAddress epAddr
  where
    splitOn :: Eq a => a -> [a] -> [[a]]
    splitOn _ [] = []
    splitOn d list =
        case r of
          [] -> [l]
          (x:xs) -> l : splitOn d xs
      where
        (l,r) = List.span (/= d) list

    (host:port:_) = splitOn ':' $ BSC.unpack addr

-- | Canonicalize HostNames
-- Note: Since some versions of Mac OS (e.g. High Sierra) IPV6 is used instead
-- of IPV4, `getHostByName` raises an exception. Therefore, we cannot resolve
-- the hostname as desired (from alias to explicit IP addr). In most cases, this
-- does not matter, but in some cases it may cause issues with container
-- orchestration for deploying and Uplink testnet.
resolveHostname :: HostName -> IO HostName
#ifdef __APPLE__
resolveHostname = pure
#else
resolveHostname = inet_ntoa . hostAddress <=< getHostByName
#endif
