{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -fno-warn-unused-type-patterns #-}

module DB.Class where

import Protolude

import Address
import Asset
import Account
import Block
import Contract
import Ledger
import Transaction

import Control.Monad.Base
import Control.Monad.Trans
import Control.Monad.Trans.Control

class MonadBase IO m => MonadDB m where
  type Conn m
  withConn :: (Conn m -> IO a) -> m a

{- A nice idea (for automatic deriving of MonadDB) but:
 -
 -  • Couldn't match type ‘Conn m’ with ‘Conn (t m)’
 -    Expected type: (Conn (t m) -> IO a) -> t m a
 -      Actual type: (Conn m -> IO a) -> t m a
 -    NB: ‘Conn’ is a type function, and may not be injective

  default withConn :: (MonadTrans t, MonadDB m1, m ~ t m1) => (Conn m1 -> IO a) -> m a
  withConn = lift . withConn
-}


-- | Instances of this class should adhere to the invariant that readDB be readonly
class MonadDB m => MonadReadDB m where

  readAsset       :: Address -> m (Either Text Asset)
  readAssets      :: m (Either Text [Asset])

  readAccount     :: Address -> m (Either Text Account)
  readAccounts    :: m (Either Text [Account])

  readContract    :: Address -> m (Either Text Contract)
  readContracts   :: m (Either Text [Contract])

  readBlock       :: Int -> m (Either Text Block)
  readLastBlock   :: m (Either Text Block)
  readBlocks      :: m (Either Text [Block])
  readLastNBlocks :: Int -> m (Either Text [Block])

  readInvalidTx   :: ByteString -> m (Either Text InvalidTransaction)
  readInvalidTxs  :: m (Either Text [InvalidTransaction])

  readWorld       :: m (Either Text World)

instance (MonadDB m) => MonadDB (ReaderT r m) where
  type Conn (ReaderT r m) = Conn m
  withConn = withConn


-- | Instances of this class should adhere to the invariant that writeDB writes to the DB
class MonadDB m => MonadWriteDB m where

  writeAsset      :: Asset -> m ()
  writeAssets     :: [Asset] -> m ()

  writeAccount    :: Account -> m ()
  writeAccounts   :: [Account] -> m ()

  writeContract   :: Contract -> m ()
  writeContracts  :: [Contract] -> m ()

  writeBlock      :: Block -> m ()
  writeBlocks     :: [Block] -> m ()

  writeInvalidTx  :: InvalidTransaction -> m ()
  writeInvalidTxs :: [InvalidTransaction] -> m ()

  resetDB         :: m (Either Text ())
  syncWorld       :: World -> m (Either Text ())

-- | Constraint Synonym for Constraint on functions that can both read and write to DB
type MonadReadWriteDB m = (MonadReadDB m, MonadWriteDB m)
