{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Script.Fixed (
  PrecN(..),
  FixedN(..),
  mkFixed,

  Fixed1(..),
  Fixed2(..),
  Fixed3(..),
  Fixed4(..),
  Fixed5(..),
  Fixed6(..),
) where

import Protolude hiding (put, get, putByteString)

import Data.Aeson
import Data.Fixed
import Data.Serialize

import SafeInteger
import Script.Pretty
import qualified Script.Token as Token
import qualified Hash

-------------------------------------------------------------------------------
-- Fixed Point Numbers
-------------------------------------------------------------------------------

-- | Type for Fixed Point number Types
data PrecN
  = Prec1
  | Prec2
  | Prec3
  | Prec4
  | Prec5
  | Prec6
  deriving (Eq, Ord, Enum, Show, Generic)

instance Serialize PrecN
instance Hashable PrecN
instance Hash.Hashable PrecN
instance NFData PrecN

-- | Types for Fixed Point Number Precision
-- Data.Fixed contains E1, E2, E3, E6

data E4

instance HasResolution E4 where
  resolution _ = 10000

data E5

instance HasResolution E5 where
  resolution _ = 100000

newtype Fixed1 = F1 (Fixed E1)
  deriving (Eq, Ord, Show, Generic, NFData, Num, Fractional, Hashable, ToJSON, FromJSON)

newtype Fixed2 = F2 (Fixed E2)
  deriving (Eq, Ord, Show, Generic, NFData, Num, Fractional, Hashable, ToJSON, FromJSON)

newtype Fixed3 = F3 (Fixed E3)
  deriving (Eq, Ord, Show, Generic, NFData, Num, Fractional, Hashable, ToJSON, FromJSON)

newtype Fixed4 = F4 (Fixed E4)
  deriving (Eq, Ord, Show, Generic, NFData, Num, Fractional, Hashable, ToJSON, FromJSON)

newtype Fixed5 = F5 (Fixed E5)
  deriving (Eq, Ord, Show, Generic, NFData, Num, Fractional, Hashable, ToJSON, FromJSON)

newtype Fixed6 = F6 (Fixed E6)
  deriving (Eq, Ord, Show, Generic, NFData, Num, Fractional, Hashable, ToJSON, FromJSON)

putFixed :: Fixed a -> Data.Serialize.Put
putFixed (MkFixed n) = put (toSafeInteger' n)

getFixed :: Data.Serialize.Get (Fixed a)
getFixed = MkFixed . fromSafeInteger <$> get

instance Serialize Fixed1 where
  put (F1 f) = putFixed f
  get = F1 <$> getFixed

instance Serialize Fixed2 where
  put (F2 f) = putFixed f
  get = F2 <$> getFixed

instance Serialize Fixed3 where
  put (F3 f) = putFixed f
  get = F3 <$> getFixed

instance Serialize Fixed4 where
  put (F4 f) = putFixed f
  get = F4 <$> getFixed

instance Serialize Fixed5 where
  put (F5 f) = putFixed f
  get = F5 <$> getFixed

instance Serialize Fixed6 where
  put (F6 f) = putFixed f
  get = F6 <$> getFixed

-- | Type for Fixed Point number values
data FixedN
  = Fixed1 Fixed1
  | Fixed2 Fixed2
  | Fixed3 Fixed3
  | Fixed4 Fixed4
  | Fixed5 Fixed5
  | Fixed6 Fixed6
  deriving (Eq, Ord, Show, Generic)

instance Hashable FixedN
instance Serialize FixedN
instance ToJSON FixedN
instance FromJSON FixedN
instance NFData FixedN

-- | Smart constructor for FixedN so it adheres to same MkFixed instantiation
mkFixed :: PrecN -> Integer -> FixedN
mkFixed prec n =
  case prec of
    Prec1 -> Fixed1 $ F1 $ MkFixed n
    Prec2 -> Fixed2 $ F2 $ MkFixed n
    Prec3 -> Fixed3 $ F3 $ MkFixed n
    Prec4 -> Fixed4 $ F4 $ MkFixed n
    Prec5 -> Fixed5 $ F5 $ MkFixed n
    Prec6 -> Fixed6 $ F6 $ MkFixed n

instance Hash.Hashable FixedN where
  toHash (Fixed1 (F1 (MkFixed n))) = Hash.toHash n
  toHash (Fixed2 (F2 (MkFixed n))) = Hash.toHash n
  toHash (Fixed3 (F3 (MkFixed n))) = Hash.toHash n
  toHash (Fixed4 (F4 (MkFixed n))) = Hash.toHash n
  toHash (Fixed5 (F5 (MkFixed n))) = Hash.toHash n
  toHash (Fixed6 (F6 (MkFixed n))) = Hash.toHash n

instance Pretty PrecN where
  ppr = ppr . \case
    Prec1 -> token Token.fixed1
    Prec2 -> token Token.fixed2
    Prec3 -> token Token.fixed3
    Prec4 -> token Token.fixed4
    Prec5 -> token Token.fixed5
    Prec6 -> token Token.fixed6

instance Pretty FixedN where
  ppr = (append "f") . ppr . \case
    Fixed1 (F1 f1) -> showFixed False f1
    Fixed2 (F2 f2) -> showFixed False f2
    Fixed3 (F3 f3) -> showFixed False f3
    Fixed4 (F4 f4) -> showFixed False f4
    Fixed5 (F5 f5) -> showFixed False f5
    Fixed6 (F6 f6) -> showFixed False f6
