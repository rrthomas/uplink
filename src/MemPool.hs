{-|

Memory pool data structures and operations for unconfirmed transactions.

-}

module MemPool (
  -- ** Types
  MemPool(..),
  emptyMemPool,

  isTxUnique,
  appendTx,
  removeTxs,
) where

import Protolude

import qualified Hash

import Data.Aeson
import Data.Aeson.Types
import Data.DList (DList)
import qualified Data.DList as DL
import Data.List (length, (\\))

import Transaction (Transaction)

-- Orphan transactions:
-- Transactions that can't go into the pool due to one or more missing input
-- transactions

-- | An unordered collection of transactions that are not in blocks in the main
-- chain, but for which we have input transactions
data MemPool = MemPool
  { size         :: Int
  , hashes       :: DList (Hash.Hash ByteString)
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
    let hs = map Hash.toHash txs
    pure $ MemPool size hs txs
  parseJSON invalid = typeMismatch "MemPool" invalid

emptyMemPool :: MemPool
emptyMemPool = MemPool 0 DL.empty DL.empty

isTxUnique :: MemPool -> Transaction -> Bool
isTxUnique (MemPool _ hs _) tx =
  let h = Hash.toHash tx in notElem h hs

-- | Appends a transaction to the mempool if the transaction is unique. If the
-- transaction already exists in the mempool, the original mempool is returned.
appendTx :: Transaction -> MemPool -> MemPool
appendTx tx mp@(MemPool s hshs txs)
  | isTxUnique mp tx = MemPool
      { size         = s + 1
      , hashes       = DL.snoc hshs $ Hash.toHash tx
      , transactions = DL.snoc txs tx
      }
  | otherwise = mp

removeTxs :: MemPool -> [Transaction] -> MemPool
removeTxs memPool txs =
    memPool
      { size         = length (newTxs :: [Transaction])
      , hashes       = DL.fromList newTxHashes
      , transactions = DL.fromList newTxs
      }
  where
    newTxs = DL.toList (transactions memPool) \\ txs
    newTxHashes = map Hash.toHash newTxs
