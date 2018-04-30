{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module DB.PostgreSQL.Block (

  -- ** Types
  BlockRow(..),

  -- ** Conversions
  blockToRowTypes,
  rowTypesToBlock,

  -- ** Queries
  queryBlock,
  queryBlocks,
  queryLastBlock,
  queryLastNBlocks,

  blockRowToBlock,
  blockRowsToBlocks,

  -- ** Inserts
  insertBlock,
  insertBlocks,

  -- ** Deletions
  deleteBlocks,
) where

import Protolude

import Address
import Block (Block(..), BlockHeader(..), BlockSignatures)
import qualified Encoding
import qualified Hash
import qualified Consensus.Authority.Params as CAP

import DB.PostgreSQL.Error
import DB.PostgreSQL.Transaction

import Database.PostgreSQL.Simple

--------------------------------------------------------------------------------
-- Types and Conversions
--------------------------------------------------------------------------------

data BlockRow = BlockRow
  { blockIndex      :: Int
  , blockOrigin     :: Address AAccount
  , blockPrevHash   :: Hash.Hash Encoding.Base16ByteString
  , blockMerkleRoot :: ByteString
  , blockTimestamp  :: Int64
  , blockConsensus  :: CAP.PoA
  , blockSignatures :: BlockSignatures
  } deriving (Generic)

instance ToRow BlockRow
instance FromRow BlockRow

blockToRowTypes :: Block -> (BlockRow, [TransactionRow])
blockToRowTypes Block{..} =
    (blockRow, transactionRows)
  where
    blockRow = BlockRow
      { blockIndex      = index
      , blockOrigin     = origin header
      , blockPrevHash   = prevHash header
      , blockMerkleRoot = merkleRoot header
      , blockTimestamp  = timestamp header
      , blockConsensus  = consensus header
      , blockSignatures = signatures
      }

    transactionRows =
      map (transactionToRowType index) transactions

rowTypesToBlock :: (BlockRow, [TransactionRow]) -> Block
rowTypesToBlock (blockRow, txRows) = do
    Block
      { index        = blockIndex blockRow
      , header       = blockHeader
      , signatures   = blockSignatures blockRow
      , transactions = transactions'
      }
  where
    transactions' = map rowTypeToTransaction txRows

    blockHeader = BlockHeader
      { origin     = blockOrigin blockRow
      , prevHash   = blockPrevHash blockRow
      , merkleRoot = blockMerkleRoot blockRow
      , timestamp  = blockTimestamp blockRow
      , consensus  = blockConsensus blockRow
      }

--------------------------------------------------------------------------------
-- Queries (SELECTs)
--------------------------------------------------------------------------------

-- | Query all blocks in the chain
queryBlocks :: Connection -> IO (Either PostgreSQLError [Block])
queryBlocks conn = do
  eBlockRows <- queryBlockRows conn
  case eBlockRows of
    Left err -> pure $ Left err
    Right blockRows ->
      fmap sequence $
        forM blockRows $ \blockRow -> do
          let blockIdx = blockIndex blockRow
          eTxRows <- queryTransactionRowsByBlock conn blockIdx
          pure $ Right . rowTypesToBlock . (blockRow,) =<< eTxRows

-- | Query a block at a given index
queryBlock :: Connection -> Int -> IO (Either PostgreSQLError Block)
queryBlock conn blockIdx = do
  eBlockRow <- queryBlockRow conn blockIdx
  case eBlockRow of
    Left err              -> pure $ Left err
    Right Nothing         -> pure $ Left $ BlockDoesNotExist blockIdx
    Right (Just blockRow) -> blockRowToBlock conn blockRow

-- | Query the last block in the chain
queryLastBlock :: Connection -> IO (Either PostgreSQLError Block)
queryLastBlock conn = do
  eBlock <- fmap headMay <$> queryLastNBlocks conn 1
  case eBlock of
    Left err         -> pure $ Left err
    Right Nothing    -> pure $ Left NoBlocksInDatabase
    Right (Just blk) -> pure $ Right blk

-- | Query the last n block in the chain
queryLastNBlocks :: Connection -> Int -> IO (Either PostgreSQLError [Block])
queryLastNBlocks conn n = do
  queryLastNBlockRows conn n >>=
    either (pure . Left) (blockRowsToBlocks conn)

blockRowToBlock :: Connection -> BlockRow -> IO (Either PostgreSQLError Block)
blockRowToBlock conn blockRow = do
  eTxRows <- queryTransactionRowsByBlock conn $ blockIndex blockRow
  pure $ Right . rowTypesToBlock . (blockRow,) =<< eTxRows

blockRowsToBlocks :: Connection -> [BlockRow] -> IO (Either PostgreSQLError [Block])
blockRowsToBlocks conn =
  fmap sequence . mapM (blockRowToBlock conn)

--------------------------------------------------------------------------------

queryBlockRow :: Connection -> Int -> IO (Either PostgreSQLError (Maybe BlockRow))
queryBlockRow conn blockIdx = fmap headMay <$>
  querySafe conn "SELECT * FROM blocks WHERE idx=?" (Only blockIdx)

queryBlockRows :: Connection -> IO (Either PostgreSQLError [BlockRow])
queryBlockRows conn =
  querySafe_ conn "SELECT * FROM blocks"

queryLastNBlockRows :: Connection -> Int -> IO (Either PostgreSQLError [BlockRow])
queryLastNBlockRows conn n =
  querySafe conn "SELECT * FROM blocks ORDER BY idx DESC LIMIT ?" (Only n)

--------------------------------------------------------------------------------
-- Inserts
--------------------------------------------------------------------------------

-- | Insert a Block into a PostgreSQL DB
-- Warning: Do not use inside another transaction
insertBlock :: Connection -> Block -> IO (Either PostgreSQLError Int64)
insertBlock conn block =
    withTransaction conn $ do
      insertBlockRow conn blockRow
      insertTransactionRows conn txRows
  where
    (blockRow, txRows) = blockToRowTypes block

insertBlocks :: Connection -> [Block] -> IO (Either PostgreSQLError [Int64])
insertBlocks conn blocks = sequence <$> mapM (insertBlock conn) blocks

insertBlockRow :: Connection -> BlockRow -> IO (Either PostgreSQLError Int64)
insertBlockRow conn blockRow =
  executeSafe conn "INSERT INTO blocks VALUES (?,?,?,?,?,?,?)" blockRow

--------------------------------------------------------------------------------
-- Deletions
--------------------------------------------------------------------------------

deleteBlocks :: Connection -> IO (Either PostgreSQLError Int64)
deleteBlocks conn = do
  eRes <- deleteTransactions conn
  case eRes of
    Left err -> pure $ Left err
    Right _  ->  executeSafe_ conn "DELETE FROM blocks"
