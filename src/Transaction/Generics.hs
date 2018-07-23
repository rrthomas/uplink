{-# LANGUAGE StrictData #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Transaction.Generics
( TxHeader(..)
, TxElem(..)
, txElemName
, txElemIndex
)
where

import GHC.Generics
import qualified Data.Text as T
import qualified Data.List as DL
import Data.Maybe
import Protolude

txElemName :: Show a => a -> TxElem
txElemName = TxElem . DL.head . T.words . show

txElemIndex :: (Show a, TxHeader a) => a -> Maybe Word16
txElemIndex e = fromIntegral <$> DL.elemIndex (txElemName e) (tHeader e)

newtype TxElem = TxElem Text
  deriving (Show, Generic, Eq)

class GTxHeader a where
  gopts :: Proxy a -> [TxElem]

gTxHeader :: forall a. (GTxHeader (Rep a)) => Proxy a -> [TxElem]
gTxHeader _ = gopts (Proxy :: Proxy (Rep a))

-- Datatype
instance GTxHeader f => GTxHeader (M1 D x f) where
  gopts _ = gopts (Proxy :: Proxy f)

-- Constructor Metadata
instance Constructor c => GTxHeader (M1 C c f) where
  gopts _ = [TxElem (toS (conName m))]
    where m = witness :: t c f a

-- Selector Metadata
instance (Selector s) => GTxHeader (M1 S s f) where
  gopts _ = [TxElem (toS (selName m))]
    where m = witness :: t s f a

-- Product branch
instance (GTxHeader a, GTxHeader b) => GTxHeader (a :*: b) where
  gopts _ = gopts (Proxy :: Proxy a) ++ gopts (Proxy :: Proxy b)

-- Constructor Paramater
instance (GTxHeader f) => GTxHeader (K1 R f) where
  gopts _ = gopts (Proxy :: Proxy f)

-- Sum branch
instance (GTxHeader a, GTxHeader b) => GTxHeader (a :+: b) where
  gopts _ = gopts (Proxy :: Proxy a) ++ gopts (Proxy :: Proxy b)

-- Void branch
instance GTxHeader U1 where
  gopts _ = []

class TxHeader a where
  tHeader :: a -> [TxElem]
  default tHeader ::  GTxHeader (Rep a) => a -> [TxElem]
  tHeader _ = gTxHeader (Proxy :: Proxy a)
