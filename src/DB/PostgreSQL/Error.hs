{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DB.PostgreSQL.Error (
  PostgreSQLError(..),

  tryPostgreSQL,

  querySafe,
  querySafe_,
  executeSafe,
  executeSafe_,
  executeManySafe,

) where

import Protolude

import Address
import Control.Exception
import Database.PostgreSQL.Simple

data PostgreSQLError
  -- Exceptions raised by Postgres
  = SqlErr SqlError
  | FmtErr FormatError
  | QueryErr QueryError
  | UnexpectedErr SomeException
  -- Specific Error types related to Uplink Schema
  | AssetDoesNotExist Address
  | AccountDoesNotExist Address
  | ContractDoesNotExist Address
  | BlockDoesNotExist Int
  | NoBlocksInDatabase
  | TransactionDoesNotExist ByteString
  | InvalidTxDoesNotExist ByteString
  -- ConversionErrors
  | AssetConversionFail Text
  | AccountConversionFail Text
  | BlockConversionFail Text
  | TxConversionFail Text
  deriving (Show)

--------------------------------------------------------------------------------
-- "Safe" versions of PostgreSQL functions
--------------------------------------------------------------------------------

tryPostgreSQL :: IO a -> IO (Either PostgreSQLError a)
tryPostgreSQL action =
    catches (Right <$> action)
      [catchSqlError, catchFmtError, catchQueryError, catchAllError]
  where
    catchSqlError   = Handler (pure . Left . SqlErr)
    catchFmtError   = Handler (pure . Left . FmtErr)
    catchQueryError = Handler (pure . Left . QueryErr)
    catchAllError   = Handler (pure . Left . UnexpectedErr)

querySafe :: (ToRow q, FromRow r) => Connection -> Query -> q -> IO (Either PostgreSQLError [r])
querySafe conn template qs =
  tryPostgreSQL $ query conn template qs

querySafe_ :: FromRow r => Connection -> Query -> IO (Either PostgreSQLError [r])
querySafe_ conn template =
  tryPostgreSQL $ query_ conn template

executeSafe :: ToRow q => Connection -> Query -> q -> IO (Either PostgreSQLError Int64)
executeSafe conn template qs =
  tryPostgreSQL $ execute conn template qs

executeSafe_ :: Connection -> Query -> IO (Either PostgreSQLError Int64)
executeSafe_ conn template =
  tryPostgreSQL $ execute_ conn template

executeManySafe :: ToRow q => Connection -> Query -> [q] -> IO (Either PostgreSQLError Int64)
executeManySafe conn template qs =
  tryPostgreSQL $ executeMany conn template qs
