{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.P2P.SignedMsg (

  -- ** Send SignedMsgs
  sendSigned,
  nsendSigned,
  nsendPeerSigned,
  nsendPeersSigned,
  nsendPeersManySigned,

  -- ** Signed, Type Channels
  SendPortSigned,
  sendChanSigned,
  RecvPortSigned,
  receiveChanTimeoutSigned,

  -- ** Receive SignedMsgs
  expectSigned,
  matchSigned,
  receiveWaitSigned
) where

import Protolude

import Control.Monad.Base (MonadBase(..))

import Control.Distributed.Process.Lifted ( NodeId, Process, ProcessId, Match
                                          , expect, SendPort, ReceivePort, sendChan
                                          , send, nsend, receiveChanTimeout)
import Control.Distributed.Process.Lifted.Class (MonadProcessBase)

import qualified Data.Binary as B

import qualified Key
import Network.P2P.Service (Service(..))
import Network.P2P.Send (receiveWaitProcBase, matchProcBase, MatchProcBase(..), nsendPeer, nsendPeers, nsendPeersMany)
import NodeState (NodeT, askNetworkAccessToken)

-- | A datatype wrapping a Network message of type `a`, and an ECDSA signature
-- of the message such that other nodes in the network can verify the message
-- sender possesses a certain shared private key.
data SignedMsg a = SignedMsg
  { message   :: a
  , signature :: Key.Signature
  } deriving (Show, Generic, Typeable, B.Binary)

-- | Makes a SignedMsg using the network access token provided in the NodeConfig
mkSignedMsg
  :: (MonadBase IO m, B.Binary a)
  => a
  -> NodeT m (SignedMsg a)
mkSignedMsg msg = do
  skey <- snd <$> askNetworkAccessToken
  liftBase $ mkSignedMsg' skey msg

-- | Makes a SignedMsg using the private key supplied
mkSignedMsg'
  :: (MonadBase IO m, B.Binary a)
  => Key.PrivateKey
  -> a
  -> m (SignedMsg a)
mkSignedMsg' skey msg = do
  sig <- liftBase $ Key.sign skey (toS $ B.encode msg)
  pure $ SignedMsg msg sig

-- | Unwraps a SignedMsg verifying the signature before returning the wrapped value
unwrapSignedMsg
  :: B.Binary a
  => Key.PubKey
  -> SignedMsg a
  -> Either InvalidSignedMsg a
unwrapSignedMsg pkey (SignedMsg msg sig)
  | Key.verify pkey sig (toS $ B.encode msg) = Right msg
  | otherwise = Left InvalidSignedMsgSignature

--------------------------------------------------------------------------------
-- Handle Signed Messages
--------------------------------------------------------------------------------

data InvalidSignedMsg
  = InvalidSignedMsgSignature
  deriving (Show)

handleSignedMsg
  :: (Monad m, B.Binary a)
  => (a -> NodeT m b)                    -- ^ Handler for underlying message
  -> SignedMsg a                         -- ^ Signed message to verify and unwrap
  -> NodeT m (Either InvalidSignedMsg b) -- ^ Result of applying the handler to the wrapped message
handleSignedMsg handler smsg = do
  netPubKey <- fst <$> askNetworkAccessToken
  handleSignedMsg' netPubKey handler smsg

handleSignedMsg'
  :: (Monad m, B.Binary a)
  => Key.PubKey                    -- ^ Public key used to verify the signed message with
  -> (a -> m b)                    -- ^ Handler for underlying message
  -> SignedMsg a                   -- ^ Signed message to verify and unwrap
  -> m (Either InvalidSignedMsg b)
handleSignedMsg' pkey handler smsg =
  case unwrapSignedMsg pkey smsg of
    Left err  -> pure $ Left err
    Right msg -> Right <$> handler msg

--------------------------------------------------------------------------------
-- Signed, Typed Channels
--------------------------------------------------------------------------------

type SendPortSigned a = SendPort (SignedMsg a)

-- | Send a message on a SignedMsg channel by using the Node's network access
-- token to sign the message before sending over the channel.
sendChanSigned
  :: (MonadProcessBase m, B.Binary a, Typeable a)
  => SendPortSigned a
  -> a
  -> NodeT m ()
sendChanSigned sp =
  sendChan sp <=< mkSignedMsg

type RecvPortSigned a = ReceivePort (SignedMsg a)

-- | Receive a message on a SignedMsg channel, verify the message was signed by
-- the network access token, and return the unwrapped, verified message.
receiveChanTimeoutSigned
  :: (MonadProcessBase m, B.Binary a, Typeable a)
  => Int
  -> RecvPortSigned a
  -> NodeT m (Maybe (Either InvalidSignedMsg a))
receiveChanTimeoutSigned t rp = do
  mSignedMsg <- receiveChanTimeout t rp
  netPubKey <- fst <$> askNetworkAccessToken
  pure $ unwrapSignedMsg netPubKey <$> mSignedMsg

--------------------------------------------------------------------------------
-- Send Signed Messages
--------------------------------------------------------------------------------

-- | Like the distributed-process function `send`, but signs the message with
-- the network-access token before sending it.
sendSigned
  :: (MonadProcessBase m, B.Binary a, Typeable a)
  => ProcessId
  -> a
  -> NodeT m ()
sendSigned pid =
  send pid <=< mkSignedMsg

-- | Like the distributed-process function `nsend`, but signs the message with
-- the network-access token before sending it to the named process in the local
-- registry.
nsendSigned
  :: (MonadProcessBase m, Service s, B.Binary a, Typeable a)
  => s -> a -> NodeT m ()
nsendSigned service =
  nsend (show service) <=< mkSignedMsg

-- | Like the distributed-process function `nsend`, but signs the message with
-- the network-access token before sending it to the named process in a remote
-- registry on the node with the supplied `NodeId`.
nsendPeerSigned
  :: (MonadProcessBase m, Service s, B.Binary a, Typeable a)
  => NodeId -> s -> a -> NodeT m ()
nsendPeerSigned service peer msg = do
  signedMsg <- mkSignedMsg msg
  nsendPeer service peer signedMsg

-- | Sends a signed message to all known peers
nsendPeersSigned
  :: (MonadProcessBase m, Service s, B.Binary a, Typeable a)
  => s -> a -> NodeT m ()
nsendPeersSigned service msg = do
  signedMsg <- mkSignedMsg msg
  nsendPeers service signedMsg

-- | Sends many signed messages to all known peers
nsendPeersManySigned
  :: (MonadProcessBase m, Service s, B.Binary a, Typeable a)
  => s -> [a] -> NodeT m ()
nsendPeersManySigned service msgs = do
  signedMsgs <- mapM mkSignedMsg msgs
  nsendPeersMany service signedMsgs

--------------------------------------------------------------------------------
-- Receive Signed Messages
--------------------------------------------------------------------------------

-- | Like the distributed-process function `expect`, but expects a message of
-- type `a` signed using network access token. This function first verifies the
-- message and then returns the unwrapped message.
expectSigned
  :: (MonadProcessBase m, Typeable a, B.Binary a)
  => NodeT m (Either InvalidSignedMsg a)
expectSigned = do
  netPubKey <- fst <$> askNetworkAccessToken
  unwrapSignedMsg netPubKey <$> expect

-- | Like the distributed-process function `match`, but matches on a message of
-- type `a` that is signed using the newtork access token. This function can be
-- used as an argument to `receiveWait` as long as a `(m a -> Process b) function
-- is supplied; Usually this would be done by leveraging the `controlP` function
-- from `distributed-process-lifted`.
matchSigned
  :: MonadProcessBase m
  => (NodeT m (Either InvalidSignedMsg a) -> Process (Either InvalidSignedMsg a))
  -> MatchProcBase (NodeT m) a
  -> Match (Either InvalidSignedMsg a)
matchSigned runInBase mpb =
  matchProcBase runInBase (toSignedMatch mpb)

-- | Wait for a SignedMsg, verifying the signature of the message and passing
-- the wrapped message to the handler functions supplied to this function via
-- MatchProcBase.
receiveWaitSigned
  :: forall m a. (MonadProcessBase m)
  => [MatchProcBase (NodeT m) a]
  -> NodeT m (Either InvalidSignedMsg a)
receiveWaitSigned = receiveWaitProcBase . map toSignedMatch

-- | Convert a MatchProcBase value to a SignedMsg MatchProcBase value
toSignedMatch
  :: MonadProcessBase m
  => MatchProcBase (NodeT m) a
  -> MatchProcBase (NodeT m) (Either InvalidSignedMsg a)
toSignedMatch (MatchProcBase f) =
  MatchProcBase (handleSignedMsg f)
