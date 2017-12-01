

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

queryAccount :: Connection -> Address -> IO (Either Text Account)
queryAccount conn addr = do
  accs <- query conn "SELECT * FROM accounts WHERE address=?" (Only addr)
  case accs of
    [acc]     -> pure $ Right acc
    otherwise -> pure $ Left $ Text.intercalate " "
      [ "PostgreSQL: Account with addr"
      , toS (rawAddr addr)
      , "does not exist."
      ]

queryAccounts :: Connection -> IO [Account]
queryAccounts conn =
  query_ conn "SELECT * FROM accounts"

insertAccount :: Connection -> Account -> IO ()
insertAccount conn acc =
  insertAccounts conn [acc]

insertAccounts :: Connection -> [Account] -> IO ()
insertAccounts conn accs = void $
  executeMany conn "INSERT INTO accounts VALUES (?,?,?,?)" accs

deleteAccount :: Connection -> Address -> IO ()
deleteAccount conn addr = void $
  execute conn "DELETE FROM accounts WHERE address=?" (Only addr)

deleteAccounts :: Connection -> IO ()
deleteAccounts conn = void $
  execute_ conn "DELETE FROM accounts"
