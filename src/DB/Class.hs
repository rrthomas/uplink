{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -fno-warn-unused-type-patterns #-}

module DB.Class where

import Protolude

import Address (Address, AAccount, AAsset, AContract)
import Asset
import Account
import Block
import Contract
import Ledger
import Transaction

import Control.Monad.Base

import qualified Encoding
import qualified Hash

-- | This type class embodies the idea of some Monad `m` having a connection to
-- a database (`DBConn m`), and on failed operations using the connection,
-- returning error values of type `DBError`. Ideally, all underlying exceptions
-- are caught and returned as `Left (DBError m)` such that when using this
-- typeclass, one does not need to handle exceptions thrown inside `withConn`.
class (Show (DBError m), MonadBase IO m) => MonadDB m where
  type DBConn m
  type DBError m
  withConn
    :: (DBConn m -> IO (Either (DBError m) a))
    -> m (Either (DBError m) a)

{- A nice idea (for automatic deriving of MonadDB) but:
 -
 -  • Couldn't match type ‘Conn m’ with ‘Conn (t m)’
 -    Expected type: (Conn (t m) -> IO a) -> t m a
 -      Actual type: (Conn m -> IO a) -> t m a
 -    NB: ‘Conn’ is a type function, and may not be injective

  default withConn :: (MonadTrans t, MonadDB m1, m ~ t m1) => (Conn m1 -> IO a) -> m a
  withConn = lift . withConn
-}

-- | If base monad is MonadDB, so is ReaderT wrapper
instance (MonadDB m) => MonadDB (ReaderT r m) where
  type DBConn (ReaderT r m) = DBConn m
  type DBError (ReaderT r m) = DBError m
  withConn = lift . withConn

-- | Instances of this class should adhere to the invariant that readDB be readonly
class MonadDB m => MonadReadDB m where

  readAsset       :: (Address AAsset) -> m (Either (DBError m) Asset)
  readAssets      :: m (Either (DBError m) [Asset])

  readAccount     :: (Address AAccount) -> m (Either (DBError m) Account)
  readAccounts    :: m (Either (DBError m) [Account])

  readContract    :: (Address AContract) -> m (Either (DBError m) Contract)
  readContracts   :: m (Either (DBError m) [Contract])

  readBlock       :: Int -> m (Either (DBError m) Block)
  readLastBlock   :: m (Either (DBError m) Block)
  readBlocks      :: m (Either (DBError m) [Block])
  readLastNBlocks :: Int -> m (Either (DBError m) [Block])

  readTransaction :: Hash.Hash Encoding.Base16ByteString -> m (Either (DBError m) Transaction)

  readInvalidTx   :: Hash.Hash Encoding.Base16ByteString -> m (Either (DBError m) InvalidTransaction)
  readInvalidTxs  :: m (Either (DBError m) [InvalidTransaction])

  readWorld       :: m (Either (DBError m) World)

-- | Instances of this class should adhere to the invariant that writeDB writes to the DB
class MonadDB m => MonadWriteDB m where

  writeAsset      :: Asset   -> m (Either (DBError m) ())
  writeAssets     :: [Asset] -> m (Either (DBError m) ())

  writeAccount    :: Account   -> m (Either (DBError m) ())
  writeAccounts   :: [Account] -> m (Either (DBError m) ())

  writeContract   :: Contract   -> m (Either (DBError m) ())
  writeContracts  :: [Contract] -> m (Either (DBError m) ())

  writeBlock      :: Block   -> m (Either (DBError m) ())
  writeBlocks     :: [Block] -> m (Either (DBError m) ())

  writeInvalidTx  :: InvalidTransaction   -> m (Either (DBError m) ())
  writeInvalidTxs :: [InvalidTransaction] -> m (Either (DBError m) ())

  resetDB         :: m (Either (DBError m) ())
  syncWorld       :: World -> m (Either (DBError m) ())

-- | Constraint Synonym for Constraint on functions that can both read and write to DB
type MonadReadWriteDB m = (MonadReadDB m, MonadWriteDB m)
