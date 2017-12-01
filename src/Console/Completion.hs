{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Console.Completion (
  completion
) where

import Protolude hiding (Prefix)

import System.Console.Repline
import qualified Data.String as String
import qualified Data.Set as Set
import qualified Data.Text as T
import Console.Lexer
import Console.Config
import qualified NodeState

defaultMatcher
  :: (MonadState ConsoleCtx m, MonadIO m) => [(String.String, CompletionFunc m)]
defaultMatcher =
  [ (toS discover   , noCompletion)
  , (toS listPeers  , noCompletion)
  , (toS reconnect  , noCompletion)
  , (toS ping       , noCompletion)
  , (toS pingPeer   , noCompletion)
  , (toS addPeer    , noCompletion)

  , (toS createAccount , fileCompleter)
  , (toS createAsset   , noCompletion)
  , (toS createContract, fileCompleter)
  , (toS transferAsset , noCompletion)
  , (toS circulateAsset, noCompletion)
  , (toS transaction, fileCompleter)
  , (toS setAccount, fileCompleter)
  ]

-- Completion
comp :: MonadState ConsoleCtx m => WordCompleter m
comp n = do
  ns <- get
  return $ filter (isPrefixOf n) $ fmap T.unpack reservedNames


noCompletion :: MonadState ConsoleCtx m => CompletionFunc m
noCompletion = listCompleter []

{-peerCompletion :: MonadState ConsoleCtx m => WordCompleter m-}
{-peerCompletion n = do-}
  {-ns <- get-}
  {-{-peers <- runNodeT' NodeState.getPeers-}-}
  {-return $ filter (isPrefixOf n) ["0.0.0.0", "bootnode"]-}

completion :: (MonadIO m, MonadState ConsoleCtx m) => CompleterStyle m
completion = Prefix (wordCompleter comp) defaultMatcher
