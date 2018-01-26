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
import Account
import Asset
import Transaction (Transaction(..), TransactionHeader(..), SyncLocalOp(..), base16HashTransaction)
import Script
import Storage
import SafeString
import SafeInteger
import qualified Transaction as TX
import Time

import DB.PostgreSQL.Error

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField

--------------------------------------------------------------------------------
-- Types and Conversions
--------------------------------------------------------------------------------

data TransactionRow = TransactionRow
  { txBlockIndex :: Int
  , txHash       :: ByteString
  , txType       :: ByteString
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
    , txType       = getTxType header
    , txHeader     = header
    , txSignature  = signature
    , txOrigin     = origin
    , txTimestamp  = timestamp
    }
  where
    getTxType txHdr =
      case txHdr of
        TxContract txc ->
          case txc of
            TX.CreateContract _ _ -> "CreateContract"
            TX.SyncLocal _ _      -> "SyncLocal"
            TX.Call _ _ _         -> "Call"
        TxAsset txa    ->
          case txa of
            TX.CreateAsset _ _ _ _ _ -> "CreateAsset"
            TX.Transfer _ _ _        -> "Transfer"
            TX.Circulate _ _         -> "Circulate"
            TX.Bind _ _ _            -> "Bind"
            TX.RevokeAsset _         -> "RevokeAsset"
        TxAccount txa  ->
          case txa of
            TX.CreateAccount _ _ _ -> "CreateAccount"
            TX.RevokeAccount _     -> "RevokeAccount"

rowTypeToTransaction :: TransactionRow -> Transaction
rowTypeToTransaction TransactionRow{..} = do
  Transaction
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
  -> IO (Either PostgreSQLError Transaction)
queryTransactionByHash conn b16txHash = do
  txRows <- querySafe conn "SELECT * from transactions where hash=?" (Only b16txHash)
  case fmap headMay txRows of
    Left err        ->
      pure $ Left err
    Right Nothing   ->
      pure $ Left $ TransactionDoesNotExist b16txHash
    Right (Just tx) ->
      pure $ Right $ rowTypeToTransaction tx

queryTransactionRowsByBlock
  :: Connection
  -> Int
  -> IO (Either PostgreSQLError [TransactionRow])
queryTransactionRowsByBlock conn blockIdx =
  querySafe conn "SELECT * FROM transactions where block_idx=?" (Only blockIdx)

--------------------------------------------------------------------------------
-- Inserts
--------------------------------------------------------------------------------

insertTransactions
  :: Connection
  -> Int
  -> [Transaction]
  -> IO (Either PostgreSQLError Int64)
insertTransactions conn blockIdx txs =
  insertTransactionRows conn $
    map (transactionToRowType blockIdx) txs

--------------------------------------------------------------------------------

insertTransactionRow
  :: Connection
  -> TransactionRow
  -> IO (Either PostgreSQLError Int64)
insertTransactionRow conn txRow = insertTransactionRows conn [txRow]

insertTransactionRows
  :: Connection
  -> [TransactionRow]
  -> IO (Either PostgreSQLError Int64)
insertTransactionRows conn txRows =
  executeManySafe conn "INSERT INTO transactions VALUES (?,?,?,?,?,?,?)" txRows

--------------------------------------------------------------------------------
-- Deletions
--------------------------------------------------------------------------------

deleteTransactions :: Connection -> IO (Either PostgreSQLError Int64)
deleteTransactions conn =
  executeSafe_ conn "DELETE FROM transactions"
