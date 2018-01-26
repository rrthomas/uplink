

module DB.PostgreSQL.Account (
  queryAccount,
  queryAccounts,

  insertAccount,
  insertAccounts,

  deleteAccount,
  deleteAccounts
) where

import Protolude

import qualified Data.Text as Text

import Account
import Address

import Database.PostgreSQL.Simple

import DB.PostgreSQL.Error

queryAccount :: Connection -> Address -> IO (Either PostgreSQLError Account)
queryAccount conn addr = do
  eAccs <- querySafe conn "SELECT * FROM accounts WHERE address=?" (Only addr)
  case fmap headMay eAccs of
    Left err         -> pure $ Left err
    Right Nothing    -> pure $ Left $ AccountDoesNotExist addr
    Right (Just acc) -> pure $ Right acc

queryAccounts
  :: Connection
  -> IO (Either PostgreSQLError [Account])
queryAccounts conn =
  querySafe_ conn "SELECT * FROM accounts"

insertAccount
  :: Connection
  -> Account
  -> IO (Either PostgreSQLError Int64)
insertAccount conn acc =
  insertAccounts conn [acc]

insertAccounts
 :: Connection
 -> [Account]
 -> IO (Either PostgreSQLError Int64)
insertAccounts conn accs =
  executeManySafe conn "INSERT INTO accounts VALUES (?,?,?,?)" accs

deleteAccount
  :: Connection
  -> Address
  -> IO (Either PostgreSQLError Int64)
deleteAccount conn addr =
  executeSafe conn "DELETE FROM accounts WHERE address=?" (Only addr)

deleteAccounts
  :: Connection
  -> IO (Either PostgreSQLError Int64)
deleteAccounts conn =
  executeSafe_ conn "DELETE FROM accounts"
