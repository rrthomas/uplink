{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.P2P.Send (

  nsendPeer,
  nsendPeers,
  nsendPeersMany,

  nsendPeer',
  nsendPeers',
  nsendPeersMany',

  MatchProcBase(..),
  matchProcBase,
  receiveWaitProcBase,

) where

import Protolude

import Control.Distributed.Process (match)
import Control.Distributed.Process.Lifted
import Control.Distributed.Process.Lifted.Class
import Control.Distributed.Process.Serializable (Serializable)

import qualified Data.Binary as B
import qualified Data.Serialize as S

import NodeState (NodeT, getPeerNodeIds)

import Network.P2P.Service (Service)
import qualified Network.P2P.Logging as Log

-------------------------------------------------------------------------------
-- P2P Messaging Utils
-------------------------------------------------------------------------------

-- | Send a message to a specific peer process
nsendPeer
  :: (MonadProcessBase m, Service s, B.Binary a, Typeable a)
  => NodeId -> s -> a -> m ()
nsendPeer nid s =
  nsendRemote nid (show s)

-- | Broadcast a message to a specific service on all peers.
nsendPeers
  :: (MonadProcessBase m, Service s, B.Binary a, Typeable a)
  => s -> a -> NodeT m ()
nsendPeers s msg = do
  peers <- getPeerNodeIds
  Log.debug $ "nsendPeers: Sending msg to " <> show (length peers) <> " peers."
  forM_ peers $ \peer -> do
    Log.debug $ "Sending msg to " <> (show peer :: Text)
    nsendPeer peer s msg

-- | Broadcast multiple messages to a specific service on all peers.
nsendPeersMany
  :: (MonadProcessBase m, Service s, B.Binary a, Typeable a)
  => s -> [a] -> NodeT m ()
nsendPeersMany s msgs = do
  peers <- getPeerNodeIds
  Log.debug $ "nsendPeers: Sending msg to " <> show (length peers) <> " peers."
  forM_ msgs $ \msg ->
    forM_ peers $ \peer -> do
      Log.debug $ "Sending msg to " <> (show peer :: Text)
      nsendPeer peer s msg

-------------------------------------------------------------------------------

-- | Like nsendPeer but serialize with Data.Serialize instead of Data.Binary
nsendPeer'
  :: (MonadProcessBase m, Service s, S.Serialize a)
  => NodeId -> s -> a -> NodeT m ()
nsendPeer' s p =
  nsendPeer s p . S.encode

-- | Like nsendPeers but serialize with Data.Serialize instead of Data.Binary
nsendPeers'
  :: (MonadProcessBase m, Service s, S.Serialize a)
  => s -> a -> NodeT m ()
nsendPeers' s =
  nsendPeers s . S.encode

-- | Like nsendPeersMany but serialize with Data.Serialize instead of Data.Binary
nsendPeersMany'
  :: (MonadProcessBase m, Service s, S.Serialize a)
  => s -> [a] -> NodeT m ()
nsendPeersMany' s msgs =
  nsendPeersMany s $ (map S.encode msgs)

-------------------------------------------------------------------------------
-- Receive Messages
-------------------------------------------------------------------------------

-- | This type mimics Control.Distributed.Process's type `Match`, but allows for
-- the matching of `MonadProcessBase m => m a` functions in `receiveWait` instead
-- of just `Process a` functions. Matches wrapped in this type are ultimately
-- converted to the Match type, where they can be used in the `receiveWait`
-- primitive. This is done because the distributed-process  library does not
-- expose many of it's intenal datatypes.
data MatchProcBase m a where
  MatchProcBase
    :: (MonadProcessBase m, Serializable b)
    => (b -> m a) -> MatchProcBase m a

-- | Like the distributed-process `match` function, this function takes a
-- modified `Match` value `MatchProcBase`, a wrapper for a function of type
-- `MonadProcessBase m => (forall b. b) -> m a`, and a function to translate an
-- `m a` value into a `Process b` value. This function can be used as an
-- argument to `receiveWait` as long as a `(m a -> Process b) function is
-- supplied; Usually this would be done by leveraging the `controlP` function
-- from `distributed-process-lifted`.
matchProcBase
  :: (m a -> Process b)
  -> MatchProcBase m a
  -> Match b
matchProcBase runInBase (MatchProcBase mpb) =
  match (runInBase . mpb)

-- | Like the distributed-process `receiveWait` function, but taking `MatchProcBase`
-- type values; i.e. this function takes a list of functions of type
-- `MonadProcessBase m => (forall b. b) -> m a` to dispatch on depending on the
-- type of the message `b` that is received.
receiveWaitProcBase
  :: MonadProcessBase m
  => [MatchProcBase m a]
  -> m a
receiveWaitProcBase ms =
  controlP $ \runInBase ->
    receiveWait $
      map (matchProcBase runInBase) ms
