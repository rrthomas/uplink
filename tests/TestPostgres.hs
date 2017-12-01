{-# LANGUAGE OverloadedStrings #-}

module TestDB (
  dbTests,
  postgresDatatypeTests,
) where

import Protolude

import Test.Tasty
import Test.QuickCheck.Monadic
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HUnit

import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS

import qualified Address
import qualified DB
import qualified Contract
import qualified Block
import qualified Time
import qualified Storage
import qualified Derivation
import qualified Script.Eval as Eval
import qualified Script.Compile as Compile

import DB.PostgreSQL.Asset
import DB.PostgreSQL.Contract
import DB.PostgreSQL.Block

import qualified Reference as Ref


withPostgresDB :: (FilePath -> IO ()) -> IO ()
withPostgresDB = bracket setUp tearDown
    where
        setUp    = undefined
        tearDown = undefined
