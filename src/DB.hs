
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DB (
  module DB.Class,

  StorageBackend(..),
) where

import Protolude

import DB.Class
import qualified DB.PostgreSQL
import qualified DB.LevelDB

import Database.PostgreSQL.Simple

-- | Storage backend
data StorageBackend
  = LevelDB FilePath
  | PostgreSQL ConnectInfo
  deriving (Eq, Show)
