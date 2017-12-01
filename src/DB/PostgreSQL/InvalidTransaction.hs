{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module DB.PostgreSQL.InvalidTransaction (

  -- ** Types
  InvalidTxRow(..),

  -- ** Conversions
  invalidTxToRowType,
  rowTypeToInvalidTx,

  -- ** Queries
  queryInvalidTxByHash,
  queryInvalidTxs,

  -- ** Inserts
  insertInvalidTx,
  insertInvalidTxs,

  -- ** Deletions
  deleteInvalidTxs,

) where

import Protolude

import qualified Data.Text as Text
import qualified Data.Serialize as S

import Address
import Transaction ( Transaction(..)
                   , InvalidTransaction(..)
                   , TransactionHeader
                   , TxValidationError
                   , base16HashInvalidTx
                   )

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField

--------------------------------------------------------------------------------
-- Types and Conversions
--------------------------------------------------------------------------------

data InvalidTxRow = InvalidTxRow
  { itxHash       :: ByteString
  , itxHeader     :: TransactionHeader
  , itxSignature  :: ByteString
  , itxOrigin     :: Address
  , itxTimestamp  :: Int64
  , itxReason     :: TxValidationError
  } deriving (Generic)

instance ToRow InvalidTxRow
instance FromRow InvalidTxRow

invalidTxToRowType :: InvalidTransaction -> InvalidTxRow
invalidTxToRowType itx@(InvalidTransaction Transaction{..} reason) =
  InvalidTxRow
    { itxHash       = base16HashInvalidTx itx
    , itxHeader     = header
    , itxSignature  = signature
    , itxOrigin     = origin
    , itxTimestamp  = timestamp
    , itxReason     = reason
    }

rowTypeToInvalidTx :: InvalidTxRow -> Either Text InvalidTransaction
rowTypeToInvalidTx InvalidTxRow{..} = do
    pure $ InvalidTransaction
      { transaction = transaction'
      , reason      = itxReason
      }
  where
    transaction' = Transaction
      { header    = itxHeader
      , signature = itxSignature
      , origin    = itxOrigin
      , timestamp = itxTimestamp
      }

--------------------------------------------------------------------------------
-- Queries (SELECTs)
--------------------------------------------------------------------------------

queryInvalidTxByHash
  :: Connection
  -> ByteString -- ^ must be base16 encoded sha3_256 hash
  -> IO (Either Text InvalidTransaction)
queryInvalidTxByHash conn b16itxHash = do
  rows <- query conn "SELECT * from invalidtxs where hash=?" (Only b16itxHash)
  case headMay rows of
    Nothing -> pure $ Left $ Text.intercalate " "
      [ "PostgreSQL: Transaction with hash"
      , toS b16itxHash
      , "does not exist."
      ]
    Just itx -> pure $ rowTypeToInvalidTx itx

queryInvalidTxs :: Connection -> IO (Either Text [InvalidTransaction])
queryInvalidTxs conn =
  sequence . map rowTypeToInvalidTx <$> queryInvalidTxRows conn

queryInvalidTxRows :: Connection -> IO [InvalidTxRow]
queryInvalidTxRows conn =
  query_ conn "SELECT * from invalidtxs"

--------------------------------------------------------------------------------
-- Inserts
--------------------------------------------------------------------------------

insertInvalidTx :: Connection -> InvalidTransaction -> IO ()
insertInvalidTx conn itx = insertInvalidTxs conn [itx]

insertInvalidTxs :: Connection -> [InvalidTransaction] -> IO ()
insertInvalidTxs conn itxs =
  insertInvalidTxRows conn $
    map invalidTxToRowType itxs

insertInvalidTxRow :: Connection -> InvalidTxRow -> IO ()
insertInvalidTxRow conn itxRow = insertInvalidTxRows conn [itxRow]

insertInvalidTxRows :: Connection -> [InvalidTxRow] -> IO ()
insertInvalidTxRows conn itxRows = void $
  executeMany conn "INSERT INTO invalidtxs hash,header,signature,origin,timestamp,reason VALUES (?,?,?,?,?,?)" itxRows

--------------------------------------------------------------------------------
-- Deletions
--------------------------------------------------------------------------------

deleteInvalidTxs :: Connection -> IO ()
deleteInvalidTxs conn = void $
  execute_ conn "DELETE FROM invalidtxs"
