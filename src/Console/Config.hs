{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Console.Config (
  ConsoleState(..),

  ConsoleT(..),
  runConsoleT,

  ConsoleM(..)
) where

import Protolude

import Control.Distributed.Process

import qualified Data.Set as Set
import qualified Data.Map as Map

import Script
import NodeState
import Account
import Key

import qualified Network.P2P.Cmd as Cmd

import System.Console.Repline
import System.Console.Haskeline.MonadException

data ConsoleState = ConsoleState {
    account :: Maybe Account
  , privKey :: Maybe Key.PrivateKey
  , vars    :: Map.Map Text Value
  } deriving (Show)

-- | General ConsoleT Monad Transformer (to get instances for freee!)
newtype ConsoleT m a = ConsoleT
  { unConsoleT :: StateT ConsoleState m a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadState ConsoleState)

-- | Necessary for using ConsoleT as a base monad for HaskelinT transformer
-- Note: This sucks, but I got it working
instance MonadException m => MonadException (ConsoleT m) where
  controlIO f =
    ConsoleT $ StateT $ \s ->
      controlIO $ \(RunIO run) ->
        let run' = RunIO (fmap (ConsoleT . StateT . const) . run . flip runStateT s . unConsoleT)
        in fmap (flip runStateT s . unConsoleT) $ f run'

runConsoleT :: Monad m => ConsoleState -> ConsoleT m a -> m a
runConsoleT consoleState = flip evalStateT consoleState . unConsoleT

-- | Monad for use in Console Program
newtype ConsoleM a = ConsoleM
  { unConsoleM :: HaskelineT (ConsoleT IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadState ConsoleState)
