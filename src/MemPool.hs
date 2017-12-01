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

import Hash

import Data.Aeson
import Data.Aeson.Types
import Data.DList (DList)
import qualified Data.DList as DL
import Data.List (length, (\\))

import Transaction (Transaction, InvalidTransaction, hashInvalidTx)

-------------------------------------------------------------------------------
-- MemPool
-------------------------------------------------------------------------------

-- | An unordered collection of transactions that are not in blocks in the main
-- chain, but for which we have input transactions
data MemPool = MemPool
  { size         :: Int
  , hashes       :: DList (Hash ByteString)
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

elemMemPool :: MemPool -> Transaction -> Bool
elemMemPool (MemPool _ hs _) tx =
  let h = Hash.toHash tx in notElem h hs

elemMemPool' :: MemPool -> ByteString -> Bool
elemMemPool' (MemPool _ hs _) txHash =
  let h = Hash.toHash txHash in notElem h hs

-- | Appends a transaction to the mempool if the transaction is unique. If the
-- transaction already exists in the mempool, the original mempool is returned.
appendTx :: Transaction -> MemPool -> MemPool
appendTx tx mp@(MemPool s hshs txs)
  | elemMemPool mp tx = MemPool
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

-------------------------------------------------------------------------------
-- Invalid Transaction Pool (Bounded)
-------------------------------------------------------------------------------

-- | Bounded pool of invalid transactions, storing the last n (`bound`)
-- transactions in memory.
data InvalidTxPool = InvalidTxPool
  { size            :: Int  -- ^ Current number of InvalidTxs in InvalidTxPool
  , bound           :: Int  -- ^ The largest number of InvalidTxs the pool can contain
  , invalidTxs      :: [InvalidTransaction]
  , invalidTxHashes :: [Hash ByteString]
  }

resetInvalidTxPool :: InvalidTxPool -> InvalidTxPool
resetInvalidTxPool itxPool =
  itxPool { size = 0, invalidTxs = [], invalidTxHashes = [] }

-- | Smart constructor for creating InvalidTxPools
mkInvalidTxPool :: Int -> Either Text InvalidTxPool
mkInvalidTxPool n
  | n <= 0    = Left "InvalidTxPool cannot be bounded by a negative integer."
  | otherwise = Right $ InvalidTxPool 0 n [] []

addInvalidTxs :: [InvalidTransaction] -> InvalidTxPool -> InvalidTxPool
addInvalidTxs newItxs itxp@(InvalidTxPool s b itxs' itxHshs') =
    itxp { size            = newSize
         , invalidTxs      = newItxs
         , invalidTxHashes = newItxHshs
         }
  where
    numNewItxs = length newItxs

    newSize' = s + numNewItxs
    newSize  = min b newSize'

    -- reverse because of order of buffer
    newItxsRev = reverse newItxs
    itxs = reverse newItxs ++ itxs'
    newItxs
      | newSize' > b = take b itxs
      | otherwise    = itxs

    itxHshs = map hashInvalidTx newItxsRev ++ itxHshs'
    newItxHshs
      | newSize' > b = take b itxHshs
      | otherwise    = itxHshs

-- Check membership of transaction in InvalidTxPool
elemInvalidTxPool :: ByteString -> InvalidTxPool -> Bool
elemInvalidTxPool itxHash itxPool =
  toHash itxHash `elem` invalidTxHashes itxPool
