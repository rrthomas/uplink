{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Console (
  runConsole,
  ConsoleConfig(..)
) where

import Protolude hiding (StateT, evalStateT, Prefix)

import Control.Distributed.Process (Process)
import Control.Monad.State.Strict

import Data.Either
import qualified Data.Map as Map
import qualified Script.Pretty as Pretty
import Script.Parser (lineContents)
import Account
import Utils
import NodeState
import qualified Data.Text as T

import Console.Completion
import Console.Command
import Console.Parser
import Console.Config

import System.Console.Repline

command :: [Char] -> Console ()
command input = 
  case parseConsoleCmd input of
    Left err -> do
      putText $ Pretty.prettyPrint err

      -- Try to match first word in input and print corresponding help text
      displayCmdHelp $ fromMaybe "" $ head $ T.words $ lineContents err
      displayActiveAccount

      
    Right cmd -> handleConsoleCmd cmd

runConsole :: ConsoleConfig -> IO ()
runConsole cfg = 
    flip evalStateT ctx 
  . flip runReaderT cfg $
  evalRepl ">>> " command [] completion (return ())
  where
    ctx = ConsoleCtx {
        account = Nothing
      , privKey = Nothing
      , vars = Map.empty
      }
