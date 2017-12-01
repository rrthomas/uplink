{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DB.PostgreSQL.Transaction (

  -- ** Types
  TransactionRow(..),

  -- ** Conversions
  transactionToRowType,
  rowTypeToTransaction,

  -- ** Queries
  queryTransactionByHash,
  queryTransactionRowsByBlock,

  -- ** Inserts
  insertTransactions,
  insertTransactionRows,

  -- ** Deletions
  deleteTransactions,
) where

import Protolude

import Control.Monad (fail)
import Control.Arrow ((&&&))

import Data.Int as Int
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Serialize as S

import Address
import Transaction (Transaction(..), TransactionHeader, base16HashTransaction)
import qualified Transaction as TX
import Time

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField

--------------------------------------------------------------------------------
-- Types and Conversions
--------------------------------------------------------------------------------

data TransactionRow = TransactionRow
  { txBlockIndex :: Int
  , txHash       :: ByteString
  , txHeader     :: TransactionHeader
  , txSignature  :: ByteString
  , txOrigin     :: Address
  , txTimestamp  :: Int64
  } deriving (Generic)

instance ToRow TransactionRow
instance FromRow TransactionRow

transactionToRowType :: Int -> Transaction -> TransactionRow
transactionToRowType blockIdx tx@Transaction{..} =
  TransactionRow
    { txBlockIndex = blockIdx
    , txHash       = base16HashTransaction tx
    , txHeader     = header
    , txSignature  = signature
    , txOrigin     = origin
    , txTimestamp  = timestamp
    }

-- XXX Validate fields?
rowTypeToTransaction :: TransactionRow -> Either Text Transaction
rowTypeToTransaction TransactionRow{..} = do
  pure $ Transaction
    { header    = txHeader
    , signature = txSignature
    , origin    = txOrigin
    , timestamp = txTimestamp
    }

--------------------------------------------------------------------------------
-- Queries (SELECTs)
--------------------------------------------------------------------------------

queryTransactionByHash
  :: Connection
  -> ByteString -- ^ must be base16 encoded sha3_256 hash
  -> IO (Either Text Transaction)
queryTransactionByHash conn b16txHash = do
  txRows <- query conn "SELECT * from transactions where hash=?" (Only b16txHash)
  case headMay txRows of
    Nothing -> pure $ Left $ Text.intercalate " "
      [ "PostgreSQL: Transaction with hash"
      , toS b16txHash
      , "does not exist."
      ]
    Just tx -> pure $ rowTypeToTransaction tx

queryTransactionRowsByBlock
  :: Connection
  -> Int
  -> IO [TransactionRow]
queryTransactionRowsByBlock conn blockIdx =
  query conn "SELECT block_idx,hash,header,signature,origin,timestamp FROM transactions where block_idx=?" (Only blockIdx)

--------------------------------------------------------------------------------
-- Inserts
--------------------------------------------------------------------------------

insertTransactions :: Connection -> Int -> [Transaction] -> IO ()
insertTransactions conn blockIdx txs =
  insertTransactionRows conn $
    map (transactionToRowType blockIdx) txs

insertTransactionRow :: Connection -> TransactionRow -> IO ()
insertTransactionRow conn txRow = insertTransactionRows conn [txRow]

insertTransactionRows :: Connection -> [TransactionRow] -> IO ()
insertTransactionRows conn txRows = void $
  executeMany conn "INSERT INTO transactions (block_idx,hash,header,signature,origin,timestamp) VALUES (?,?,?,?,?,?)" txRows

--------------------------------------------------------------------------------
-- Deletions
--------------------------------------------------------------------------------

deleteTransactions :: Connection -> IO ()
deleteTransactions conn = void $
  execute_ conn "DELETE FROM transactions"
