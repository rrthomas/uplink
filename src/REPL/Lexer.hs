{-# OPTIONS_GHC -fno-warn-missing-signatures  #-}

module REPL.Lexer (
  setSender,
  setTimestamp,
  help,
  methods,
  assets,
  contract,
  ledger,

  reservedNames,
) where

import Data.Text (Text)

setSender = ":set sender"
setTimestamp = ":set timestamp"

methods = ":methods"
assets = ":assets"
contract = ":contract"
ledger = ":ledger"

help = ":help"

reservedNames :: [Text]
reservedNames = [
    setSender
  , setTimestamp

  , methods
  , assets
  , contract
  , ledger

  , help
  ]
