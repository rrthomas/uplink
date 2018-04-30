{-|

Memory pool data structures and operations for unconfirmed transactions.

-}

{-# LANGUAGE DuplicateRecordFields #-}

module MemPool (
  -- ** MemPool
  MemPool(..),
  emptyMemPool,

  elemMemPool,
  elemMemPool',
  appendTx,
  removeTxs,

  -- ** InvalidTxPool
  InvalidTxPool,
  resetInvalidTxPool,
  mkInvalidTxPool,
  addInvalidTxs,

  elemInvalidTxPool,
) where

import Protolude

import Data.Aeson
import Data.Aeson.Types
import Data.DList (DList)
import qualified Data.DList as DL
import qualified Encoding as E
import Data.List (length, (\\))

import qualified Hash as H
import Transaction (Transaction, InvalidTransaction, hashTransaction, hashInvalidTx)

-------------------------------------------------------------------------------
-- MemPool
-------------------------------------------------------------------------------

-- | An unordered collection of transactions that are not in blocks in the main
-- chain, but for which we have input transactions
data MemPool = MemPool
  { size         :: Int
  , hashes       :: DList (H.Hash E.Base16ByteString)
  , transactions :: DList Transaction
  } deriving Show

-- Issues encoding hashes of txs
instance ToJSON MemPool where
  toJSON (MemPool s _ ts) =
    object [ "size"         .= s
           , "transactions" .= ts
           ]

instance FromJSON MemPool where
  parseJSON (Object v) = do
    size <- v .: "size"
    txs <- v .: "transactions"
    let hs = map hashTransaction txs
    pure $ MemPool size hs txs
  parseJSON invalid = typeMismatch "MemPool" invalid

emptyMemPool :: MemPool
emptyMemPool = MemPool 0 DL.empty DL.empty

-- | Returns True if transaction exists in the mempool
elemMemPool :: MemPool -> Transaction -> Bool
elemMemPool mp = elemMemPool' mp . hashTransaction

-- | Returns True if a transaction with the given base16 tx hash exists in the mempool
-- Note: Must be a Base16 encoded bytestring, as this is how txs are identified.
elemMemPool' :: MemPool -> H.Hash E.Base16ByteString -> Bool
elemMemPool' (MemPool _ hs _) txHash = elem txHash hs

-- | Appends a transaction to the mempool if the transaction is unique.
appendTx :: Transaction -> MemPool -> Maybe MemPool
appendTx tx mp@(MemPool s hshs txs)
  | elemMemPool mp tx = Nothing
  | otherwise =
      Just $ MemPool
        { size         = s + 1
        , hashes       = DL.snoc hshs $ hashTransaction tx
        , transactions = DL.snoc txs tx
        }

removeTxs :: MemPool -> [Transaction] -> MemPool
removeTxs memPool txs =
    memPool
      { size         = length (newTxs :: [Transaction])
      , hashes       = DL.fromList newTxHashes
      , transactions = DL.fromList newTxs
      }
  where
    newTxs = DL.toList (transactions memPool) \\ txs
    newTxHashes = map hashTransaction newTxs

-------------------------------------------------------------------------------
-- Invalid Transaction Pool (Bounded)
-------------------------------------------------------------------------------

-- | Bounded pool of invalid transactions, storing the last n (`bound`)
-- transactions in memory.
data InvalidTxPool = InvalidTxPool
  { itxPoolSize   :: Int  -- ^ Current number of InvalidTxs in InvalidTxPool
  , itxPoolBound  :: Int  -- ^ The largest number of InvalidTxs the pool can contain
  , itxPoolTxs    :: [InvalidTransaction]
  , itxPoolHashes :: [H.Hash E.Base16ByteString]
  } deriving (Show)

resetInvalidTxPool :: InvalidTxPool -> InvalidTxPool
resetInvalidTxPool itxPool =
  itxPool { itxPoolSize = 0, itxPoolTxs = [], itxPoolHashes = [] }

-- | Smart constructor for creating InvalidTxPools
mkInvalidTxPool :: Int -> Either Text InvalidTxPool
mkInvalidTxPool n
  | n <= 0    = Left "InvalidTxPool cannot be bounded by a negative integer."
  | otherwise = Right $ InvalidTxPool 0 n [] []

addInvalidTxs :: [InvalidTransaction] -> InvalidTxPool -> InvalidTxPool
addInvalidTxs newItxs itxp@(InvalidTxPool origSize bound origItxs origItxHshs) =
  itxp { itxPoolSize   = finalNewSize
       , itxPoolTxs    = finalNewItxs
       , itxPoolHashes = finalNewItxHshs
       }
  where
    -- reverse because of order of buffer
    newItxsRev = reverse newItxs
    itxs = newItxsRev ++ origItxs

    newSize = origSize + (length newItxs)
    finalNewSize  = min bound newSize

    finalNewItxs = take finalNewSize itxs

    itxHshs = map hashInvalidTx newItxsRev ++ origItxHshs
    finalNewItxHshs = take finalNewSize itxHshs

-- Check membership of transaction in InvalidTxPool
elemInvalidTxPool :: H.Hash E.Base16ByteString -> InvalidTxPool -> Bool
elemInvalidTxPool itxHash itxPool =
  itxHash `elem` itxPoolHashes itxPool
