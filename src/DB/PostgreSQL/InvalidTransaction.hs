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

import qualified Encoding
import qualified Hash
import Address
import Transaction ( Transaction(..)
                   , InvalidTransaction(..)
                   , TransactionHeader
                   , TxValidationError
                   , hashInvalidTx
                   )

import DB.PostgreSQL.Error

import Database.PostgreSQL.Simple

--------------------------------------------------------------------------------
-- Types and Conversions
--------------------------------------------------------------------------------

data InvalidTxRow = InvalidTxRow
  { itxHash       :: Hash.Hash Encoding.Base16ByteString
  , itxHeader     :: TransactionHeader
  , itxSignature  :: Encoding.Base64PByteString
  , itxOrigin     :: Address AAccount
  , itxReason     :: TxValidationError
  } deriving (Generic)

instance ToRow InvalidTxRow
instance FromRow InvalidTxRow

invalidTxToRowType :: InvalidTransaction -> InvalidTxRow
invalidTxToRowType itx@(InvalidTransaction Transaction{..} reason) =
  InvalidTxRow
    { itxHash       = hashInvalidTx itx
    , itxHeader     = header
    , itxSignature  = signature
    , itxOrigin     = origin
    , itxReason     = reason
    }

rowTypeToInvalidTx :: InvalidTxRow -> InvalidTransaction
rowTypeToInvalidTx InvalidTxRow{..} = do
    InvalidTransaction
      { transaction = transaction'
      , reason      = itxReason
      }
  where
    transaction' = Transaction
      { header    = itxHeader
      , signature = itxSignature
      , origin    = itxOrigin
      }

--------------------------------------------------------------------------------
-- Queries (SELECTs)
--------------------------------------------------------------------------------

queryInvalidTxByHash
  :: Connection
  -> Hash.Hash Encoding.Base16ByteString -- ^ must be base16 encoded sha3_256 hash
  -> IO (Either PostgreSQLError InvalidTransaction)
queryInvalidTxByHash conn hash = do
  eRows <- querySafe conn "SELECT * FROM invalidtxs WHERE hash=?" (Only (Hash.getRawHash hash))
  case fmap headMay eRows of
    Left err         -> pure $ Left err
    Right Nothing    -> pure $ Left $ InvalidTxDoesNotExist hash
    Right (Just itx) -> pure $ Right $ rowTypeToInvalidTx itx

queryInvalidTxs :: Connection -> IO (Either PostgreSQLError [InvalidTransaction])
queryInvalidTxs conn =
  second (map rowTypeToInvalidTx) <$> queryInvalidTxRows conn

queryInvalidTxRows :: Connection -> IO (Either PostgreSQLError [InvalidTxRow])
queryInvalidTxRows conn =
  querySafe_ conn "SELECT * FROM invalidtxs"

--------------------------------------------------------------------------------
-- Inserts
--------------------------------------------------------------------------------

insertInvalidTx :: Connection -> InvalidTransaction -> IO (Either PostgreSQLError Int64)
insertInvalidTx conn itx = insertInvalidTxs conn [itx]

insertInvalidTxs :: Connection -> [InvalidTransaction] -> IO (Either PostgreSQLError Int64)
insertInvalidTxs conn itxs =
  insertInvalidTxRows conn $
    map invalidTxToRowType itxs

insertInvalidTxRow :: Connection -> InvalidTxRow -> IO (Either PostgreSQLError Int64)
insertInvalidTxRow conn itxRow = insertInvalidTxRows conn [itxRow]

insertInvalidTxRows :: Connection -> [InvalidTxRow] -> IO (Either PostgreSQLError Int64)
insertInvalidTxRows conn itxRows =
  executeManySafe conn "INSERT INTO invalidtxs VALUES (?,?,?,?,?)" itxRows

--------------------------------------------------------------------------------
-- Deletions
--------------------------------------------------------------------------------

deleteInvalidTxs :: Connection -> IO (Either PostgreSQLError Int64)
deleteInvalidTxs conn =
  executeSafe_ conn "DELETE FROM invalidtxs"
