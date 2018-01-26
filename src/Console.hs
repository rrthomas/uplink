{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Console (
  runConsole,
) where

import Protolude hiding (StateT, evalStateT, Prefix)

import Control.Distributed.Process (Process, ProcessId)
import Control.Distributed.Process.Node (LocalNode, runProcess)
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

handleConsoleInput
  :: LocalNode       -- Cloud Haskell Node to run the process on
  -> ProcessId  -- Command Process to send the message to
  -> [Char]
  -> ConsoleM ()
handleConsoleInput node cmdProcId input =
  case parseConsoleCmd input of
    Left err -> do
      putText $ Pretty.prettyPrint err
      -- Try to match first word in input and print corresponding help text
      displayCmdHelp $ fromMaybe "" $ head $ T.words $ lineContents err
      displayActiveAccount

    Right ccmd -> do
      mCmd <- getCmd ccmd
      case mCmd of
        Nothing -> pure ()
        Just cmd ->
          -- Handle the command
          liftIO $ runProcess node $
            handleConsoleCmd cmdProcId cmd

runConsole :: LocalNode -> ProcessId -> ConsoleState -> IO ()
runConsole node cmdProcId consoleState =
    runConsoleT consoleState $
      evalRepl ">>> " handleConsoleInput' [] completion (pure ())
  where
    handleConsoleInput' = unConsoleM . handleConsoleInput node cmdProcId
