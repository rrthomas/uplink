{-# LANGUAGE MultiParamTypeClasses #-}

module Console.Config (
  ConsoleConfig(..),
  ConsoleCtx(..),
  Console,
) where

import Protolude hiding (StateT, evalStateT, Prefix)

import Control.Monad.Base
import System.Console.Repline
import Control.Distributed.Process (Process)
import Control.Monad.State.Strict
import qualified Data.Set as Set
import qualified Data.Map as Map
import Script
import NodeState
import Data.String as String
import Account
import Key

import qualified Network.P2P.Cmd as Cmd

data ConsoleConfig = ConsoleConfig {
    chan       :: Chan Cmd.Cmd
  , resultChan :: Chan Cmd.CmdResult
  }

data ConsoleCtx = ConsoleCtx {
    account :: Maybe Account
  , privKey :: Maybe Key.PrivateKey
  , vars :: Map.Map Text Value
  } deriving (Show)

type Console a = HaskelineT (ReaderT ConsoleConfig (StateT ConsoleCtx IO)) a
