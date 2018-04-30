{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Utils (

  -- ** Low-Level Networking
  resolve,

  mkNodeId,
  extractNodeId,

  -- ** Service Communication
  findLocalService,
  waitForLocalService,
  waitForLocalService',

  findRemoteService,

  commProc,

  -- ** Generic
  timeout,

) where

import Protolude hiding (newChan, waitEitherCatch, withAsync)

import Control.Monad.Base
import Control.Monad.Trans.Control

import Control.Concurrent.Async.Lifted (withAsync, waitEitherCatch)

import Control.Distributed.Process.Lifted
import Control.Distributed.Process.Lifted.Class

import qualified Data.Binary as B
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as List
import qualified Data.IP as IP

import qualified Text.Hostname as TH

import Network.Socket
  ( getAddrInfo, getNameInfo
  , HostName, ServiceName, AddrInfo(..)
  , NameInfoFlag(..))

import Network.P2P.Service

import Network.Transport (EndPointAddress(..))

--------------------------------------------------------------------------------
-- Low-Level Networking
--------------------------------------------------------------------------------

-- | Canonicalize (HostName, Port) using getAddrInfo instead of *obselete*
-- getHostByName, according to the linux man pages:
-- http://man7.org/linux/man-pages/man3/gethostbyname.3.html#Description.
--
-- This function takes a user defined (HostName,ServiceName) pair, and
-- canonicalizes the pair using UNIX getaddrinfo and then getnameinfo for
-- a round trip to the OS and back such that the pair is guaranteed to be
-- in the form (IP ADDRESS, VALID NUMERICAL PORT)
--
-- The FFI calls to `getaddrinfo` and `getnameinfo` need to be wrapped in an
-- async safe timeout function because in certain cases the FFI call to these C
-- functions blocks the main thread preventing the uplink node from running.
resolve
  :: (HostName,ServiceName)
  -> IO (Either Text (HostName, ServiceName))
resolve (host,port) = do
  eSockAddr <- resolveSockAddr
  case eSockAddr of
    Left err -> pure $ Left err
    Right sockAddr -> getResolvedAddr sockAddr
  where
    -- Get the SockAddr of a (HostName, ServiceName) pair by calling the
    -- Unix C function `getaddrinfo`
    resolveSockAddr = do
      mSockAddr <-
        timeout 100000 $
          getAddrInfo Nothing (Just host) (Just port)
      pure $
        case mSockAddr of
          Nothing             -> Left $ "`getaddrinfo` timed out on: " <> show (host,port)
          Just (Left err)     -> Left $ show err
          Just (Right [])     -> Left $ "`getaddrinfo` returned 0 socket addresses for: " <> show (host,port)
          Just (Right (a:as)) -> Right $ addrAddress a

    -- Get the IP Address and Port number such that
    --   - The HostName is in IPvX format
    --   - The ServiceName (PortNumber) is a numerical value
    getResolvedAddr sockAddr = do
      mNameInfo <-
        timeout 100000 $
          getNameInfo [NI_NUMERICSERV, NI_NUMERICHOST] True True sockAddr
      pure $
        case mNameInfo of
          Nothing          -> Left $ "`getnameinfo` timed out on: " <> show sockAddr
          Just (Left err)  -> Left $ show err
          Just (Right res) ->
            case res of
              (Nothing, Nothing) ->
                Left $ "`getnameinfo` could not get the host service names from socket address: " <> show sockAddr
              (Just _, Nothing)  ->
                Left $ "Failed to resolve hostname: " <> toS host
              (Nothing, Just _)  ->
                Left $ "Failed to resolve port number: " <> toS port
              (Just hostname, Just portNum) ->
                Right (hostname, portNum)

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
mkNodeId address = do
  case parseHostPort address of
    Left err          -> pure $ Left err
    Right hostPort -> do
      eHostPort <- resolve hostPort
      case eHostPort of
        Left err -> pure $ Left err
        Right (hostname, port) -> do
          let isValidHost = validateHostname hostname
          if isValidHost
            then do
              let epAddr = BSC.pack $ intercalate ":" [hostname, port, "0"]
              return $ Right $
                NodeId $ EndPointAddress epAddr
            else
              return $ Left $ "Invalid node address: " <> show address

parseHostPort :: ByteString -> Either Text (HostName, ServiceName)
parseHostPort addr =
  let addr' = BSC.unpack addr in
  case splitOn ':' (BSC.unpack addr) of
    (host:port:_) -> Right (host,port)
    otherwise     ->
      let errStr = "'" <> show addr' <> "' is not a valid node address:\n  "
                <> "Node addresses must be of the form '<hostname>:<port>'.'"
       in Left errStr
 where
    splitOn :: Eq a => a -> [a] -> [[a]]
    splitOn _ [] = []
    splitOn d list =
        case r of
          [] -> [l]
          (x:xs) -> l : splitOn d xs
      where
        (l,r) = List.span (/= d) list

--------------------------------------------------------------------------------
-- Service Communication
--------------------------------------------------------------------------------

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
  :: (MonadProcessBase m, B.Binary a, Typeable a, B.Binary b, Typeable b)
  => ProcessId
  -> a
  -> m b
commProc pid msg = do
  (sp,rp) <- newChan
  send pid (sp, msg)
  receiveChan rp

--------------------------------------------------------------------------------
-- Generic
--------------------------------------------------------------------------------

-- This function provides a monad-agnostic timeout behavior that solves the
-- problem of the canonical System.Timeout.timeout function not being able to
-- interrupt FFI calls that have not yet returned. Returns `Nothing` if the
-- function times out, `Left SomeException` if the callee throws an exception
-- during evaluation, and `Right a` if the callee returns a value before the
-- timeout.
timeout
  :: MonadBaseControl IO m
  => Int
  -> m a
  -> m (Maybe (Either SomeException a))
timeout n right =
  withAsync (delay n) $ \a ->
    withAsync right $ \b -> do
      eRes <- waitEitherCatch a b
      pure $ case eRes of
        -- The timeout won the race
        Left (Right ()) -> Nothing
        Left (Left _)   -> Nothing
        -- The callee won the race, but threw an exception
        Right (Left e)  -> Just (Left e)
        -- The callee won the race, and returned a value
        Right (Right v) -> Just (Right v)
  where
    delay n = liftBase $ threadDelay n
