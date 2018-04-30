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

import Address
import qualified Encoding
import qualified Hash
import Transaction (Transaction(..), TransactionHeader(..), hashTransaction)
import qualified Transaction as TX

import DB.PostgreSQL.Error

import Database.PostgreSQL.Simple

--------------------------------------------------------------------------------
-- Types and Conversions
--------------------------------------------------------------------------------

data TransactionRow = TransactionRow
  { txBlockIndex :: Int
  , txHash       :: Hash.Hash Encoding.Base16ByteString
  , txType       :: ByteString
  , txHeader     :: TransactionHeader
  , txSignature  :: Encoding.Base64PByteString
  , txOrigin     :: Address AAccount
  } deriving (Generic)

instance ToRow TransactionRow
instance FromRow TransactionRow

transactionToRowType :: Int -> Transaction -> TransactionRow
transactionToRowType blockIdx tx@Transaction{..} =
  TransactionRow
    { txBlockIndex = blockIdx
    , txHash       = hashTransaction tx
    , txType       = getTxType header
    , txHeader     = header
    , txSignature  = signature
    , txOrigin     = origin
    }
  where
    getTxType txHdr =
      case txHdr of
        TxContract txc ->
          case txc of
            TX.CreateContract _ -> "CreateContract"
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
    }

--------------------------------------------------------------------------------
-- Queries (SELECTs)
--------------------------------------------------------------------------------

queryTransactionByHash
  :: Connection
  -> Hash.Hash Encoding.Base16ByteString -- ^ must be base16 encoded sha3_256 hash
  -> IO (Either PostgreSQLError Transaction)
queryTransactionByHash conn hash = do
  txRows <- querySafe conn "SELECT * from transactions where hash=?" (Only hash)
  case fmap headMay txRows of
    Left err        ->
      pure $ Left err
    Right Nothing   ->
      pure $ Left $ TransactionDoesNotExist hash
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
  executeManySafe conn "INSERT INTO transactions VALUES (?,?,?,?,?,?)" txRows

--------------------------------------------------------------------------------
-- Deletions
--------------------------------------------------------------------------------

deleteTransactions :: Connection -> IO (Either PostgreSQLError Int64)
deleteTransactions conn =
  executeSafe_ conn "DELETE FROM transactions"
