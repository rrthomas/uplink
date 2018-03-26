{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Utils (
  mkNodeId,
  extractNodeId,

  findLocalService,
  waitForLocalService,
  waitForLocalService',

  findRemoteService,
  resolveHostname,

  commProc,
) where

import Protolude hiding (newChan)

import Control.Distributed.Process.Lifted
import Control.Distributed.Process.Lifted.Class
import Control.Distributed.Process.Serializable (Serializable)

import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as List
import qualified Data.IP as IP

import qualified Text.Hostname as TH

#ifndef OS_Mac
import Network.BSD (getHostByName, hostAddress)
import Network.Socket (inet_ntoa)
#endif

import Network.P2P.Service

import Network.BSD (HostName)
import Network.Transport (EndPointAddress(..))

extractNodeId :: MonadProcessBase m => m ByteString
extractNodeId = do
  -- nodeId is of "nid://ip:port:x"
  nodeId <- liftA show getSelfNode
  let len = length ("nid://" :: [Char])
  let nodeId' = BSC.drop len nodeId
  -- ip = "ip", rest = ":port:x"
  let (ip, rest) = BSC.break (==':') nodeId'
  -- port = "port", _ = ":x"
  let (port, _)  = BSC.break (==':') $ BSC.tail rest

  return $ ip <> ":" <> port

-- | Make a NodeId from "host:port" string.
mkNodeId :: ByteString -> IO (Either Text NodeId)
mkNodeId addr = do
  case eHostPort of
    Left err          -> pure $ Left err
    Right (host,port) -> do
      let isValidHost = validateHostname host
      if isValidHost
        then do
          hostname <- resolveHostname host
          let epAddr = BSC.pack $ intercalate ":" [hostname, port, "0"]
          return $ Right $
            NodeId $ EndPointAddress epAddr
        else
          return $ Left $
            "Invalid node address: " <> show addr
 where
    splitOn :: Eq a => a -> [a] -> [[a]]
    splitOn _ [] = []
    splitOn d list =
        case r of
          [] -> [l]
          (x:xs) -> l : splitOn d xs
      where
        (l,r) = List.span (/= d) list

    eHostPort =
      let addr' = BSC.unpack addr in
      case splitOn ':' (BSC.unpack addr) of
        (host:port:_) -> Right (host,port)
        otherwise     ->
          let errStr = "'" <> show addr' <> "' is not a valid node address:\n  "
                    <> "Node addresses must be of the form '<hostname>:<port>'.'"
           in Left errStr

-- | Find a service on the local node within the given timeout
findLocalService
  :: MonadProcessBase m
  => Service
  -> Int
  -> m (Maybe ProcessId)
findLocalService service timeout = do
  let serviceNm = show service
  whereis serviceNm >>= \case
    Nothing -> do
      let timeleft = timeout - 10000
      if timeleft <= 0
         then pure Nothing
         else do
           liftIO (threadDelay 10000)
           findLocalService service timeout
    Just pid -> pure $ Just pid

-- | Waits for local service to spawn for the given time before spawning the
-- process passed as an argument.
waitForLocalService
  :: MonadProcessBase m
  => Service
  -> Int
  -> m a
  -> m (Maybe a)
waitForLocalService service timeout proc = do
  mPid <- findLocalService service timeout
  case mPid of
    Nothing -> pure Nothing
    Just _  -> Just <$> proc

-- | Like `waitForLocalService` but terminates the calling process if the
-- service is not found in the given timeout.
waitForLocalService'
  :: MonadProcessBase m
  => Service
  -> Int
  -> m a
  -> m a
waitForLocalService' service timeout proc = do
  mres <- waitForLocalService service timeout proc
  case mres of
    Nothing -> terminate
    Just x  -> pure x

validateHostname :: [Char] -> Bool
validateHostname host =
  case readMaybe host of
    Just (_ :: IP.IPv4) -> True
    Nothing             ->
      case readMaybe host of
        Just (_ :: IP.IPv6) -> True
        Nothing             ->
          TH.validHostname $ toS host


-- | Find the process id of a service on a remote node
findRemoteService
  :: MonadProcessBase m
  => NodeId
  -> Service
  -> m ProcessId
findRemoteService nodeId service = do
  whereisRemoteAsync nodeId (show service)
  reply <- expectTimeout 1500000
  case reply of
    Just (WhereIsReply tasks (Just pid)) ->
      return pid
    _ -> do
      putText $ "Failed to connect to " <> show service <> " service at: " <> show nodeId
      findRemoteService nodeId service

-- | Send a message and block for response. Service agnostic
commProc
  :: (MonadProcessBase m, Serializable a, Serializable b)
  => ProcessId
  -> a
  -> m b
commProc pid msg = do
  (sp,rp) <- newChan
  send pid (sp, msg)
  receiveChan rp

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
