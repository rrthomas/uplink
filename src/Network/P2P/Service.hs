{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.P2P.Service (
  Service(..),
) where

import Protolude

import Prelude (show)

import Control.Distributed.Process.Serializable
import Data.Binary
import Data.Serialize


{-

This module defines the different services (distinct processes) that
each node in the P2P network may run. Not all services are present on
each node, as some nodes have different permissions and/or roles than
others.

The main services run by all nodes are:
  Logger
  Tasks
  PeerController
  Messaging
  Simulation

Services run by some nodes:
  TestMessaging
  Consensus

-}

data Service
  = Logger          -- ^ Logging process
  | Tasks           -- ^ Command handler process
  | PeerController  -- ^ Peer discovery process
  | Consensus       -- ^ Consensus process for new block creation
  | Messaging       -- ^ Message handling process
  | TestMessaging   -- ^ TestMessage handling process
  | Simulation      -- ^ Simulate smart contract method evalution
  deriving (Eq, Ord, Generic, Binary, Serializable, Serialize)

instance Show Service where
  show Logger = "logger"
  show Tasks = "tasks"
  show PeerController = "p2p:controller"
  show Consensus = "consensus"
  show Messaging = "comm"
  show TestMessaging = "test-comm"
  show Simulation = "simulation"
