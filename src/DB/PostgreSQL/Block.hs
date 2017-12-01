{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

  -- ** Inserts
  insertBlock,
  insertBlocks,

  -- ** Deletions
  deleteBlocks,
) where

import Protolude

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Serialize as S

import Address
import Block (Block(..), BlockHeader(..), BlockSignatures)
import Time
import qualified Consensus.Authority.Params as CAP

import DB.PostgreSQL.Transaction

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow

--------------------------------------------------------------------------------
-- Types and Conversions
--------------------------------------------------------------------------------

data BlockRow = BlockRow
  { blockIndex      :: Int
  , blockOrigin     :: Address
  , blockPrevHash   :: ByteString
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

rowTypesToBlock :: (BlockRow, [TransactionRow]) -> Either Text Block
rowTypesToBlock (blockRow, txRows) = do
    transactions' <- sequence $ map rowTypeToTransaction txRows
    pure $ Block
      { index        = blockIndex blockRow
      , header       = blockHeader
      , signatures   = blockSignatures blockRow
      , transactions = transactions'
      }
  where
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
queryBlocks :: Connection -> IO (Either Text [Block])
queryBlocks conn = do
  blockRows <- queryBlockRows conn
  fmap sequence $
    forM blockRows $ \blockRow -> do
      let blockIdx = blockIndex blockRow
      txRows <- queryTransactionRowsByBlock conn blockIdx
      pure $ rowTypesToBlock (blockRow, txRows)

-- | Query a block at a given index
queryBlock :: Connection -> Int -> IO (Either Text Block)
queryBlock conn blockIdx = do
  mBlockRow <- queryBlockRow conn blockIdx
  case mBlockRow of
    Nothing -> pure $ Left $ Text.intercalate " "
      [ "PostgreSQL: Block with index"
      , show blockIdx
      , "does not exist."
      ]
    Just blockRow -> do
      txRows <- queryTransactionRowsByBlock conn blockIdx
      pure $ rowTypesToBlock (blockRow, txRows)

-- | Query the last block in the chain
queryLastBlock :: Connection -> IO (Either Text Block)
queryLastBlock conn = do
  eBlock <- fmap headMay <$> queryLastNBlocks conn 1
  case eBlock of
    Left err         -> pure $ Left err
    Right Nothing    -> pure $ Left
      "queryLastBlock: no blocks in table 'blocks'."
    Right (Just blk) -> pure $ Right blk

-- | Query the last n block in the chain
queryLastNBlocks :: Connection -> Int -> IO (Either Text [Block])
queryLastNBlocks conn n = do
  blockRows <- queryLastNBlockRows conn n
  fmap sequence $ forM blockRows $ \blockRow -> do
    txRows <- queryTransactionRowsByBlock conn (blockIndex blockRow)
    pure $ rowTypesToBlock (blockRow, txRows)

--------------------------------------------------------------------------------

queryBlockRow :: Connection -> Int -> IO (Maybe BlockRow)
queryBlockRow conn blockIdx = headMay <$>
  query conn "SELECT * FROM blocks WHERE idx=?" (Only blockIdx)

queryBlockRows :: Connection -> IO [BlockRow]
queryBlockRows conn = do
  query_ conn "SELECT * FROM blocks"

queryLastNBlockRows :: Connection -> Int -> IO [BlockRow]
queryLastNBlockRows conn n =
  query conn "SELECT * FROM blocks ORDER BY idx DESC LIMIT ?" (Only n)

--------------------------------------------------------------------------------
-- Inserts
--------------------------------------------------------------------------------

-- | Insert a Block into a PostgreSQL DB
-- Warning: Do not use inside another transaction
insertBlock :: Connection -> Block -> IO ()
insertBlock conn block =
    withTransaction conn $ do
      insertBlockRow conn blockRow
      insertTransactionRows conn txRows
  where
    (blockRow, txRows) = blockToRowTypes block

insertBlocks :: Connection -> [Block] -> IO ()
insertBlocks conn blocks = mapM_ (insertBlock conn) blocks

insertBlockRow :: Connection -> BlockRow -> IO ()
insertBlockRow conn blockRow = void $
  execute conn "INSERT INTO blocks VALUES (?,?,?,?,?,?,?)" blockRow

--------------------------------------------------------------------------------
-- Deletions
--------------------------------------------------------------------------------

deleteBlocks :: Connection -> IO ()
deleteBlocks conn = void $ do
  deleteTransactions conn
  execute_ conn "DELETE FROM blocks"
