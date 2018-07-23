{-|

Storage for deployed contracts.

--}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Storage (
  -- ** Storage
  Key(..),
  Value(..),
  Storage,
  GlobalStorage(..),
  LocalStorage(..),

  -- ** Serialization
  decodeStorage,
  decodeLocalStorage,
  encodeStorage,
  storageSize,

  -- ** Validation
  validateStorage,

  -- ** Hashing
  hashStorage,

) where

import Protolude hiding (Type)

import Script (Value(..), DateTime(..))
import Script.Pretty (Pretty(..))

import Crypto.Number.Serialize (os2ip)

import qualified Encoding
import qualified Hash

import Datetime.Types

import Data.Scientific
import Data.Serialize as S (Serialize, encode, decode, put, get)
import Data.Aeson (FromJSONKey(..), ToJSONKey(..), ToJSON(..), FromJSON(..), object, (.=), (.:))
import Data.Aeson.Types (typeMismatch, toJSONKeyText)
import qualified Data.Aeson as A
import qualified Data.Map as Map

import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

newtype Key = Key { unKey :: Text }
  deriving (Eq, Show, Generic, Ord, NFData, IsString)

type Storage = Map.Map Key Value

newtype GlobalStorage = GlobalStorage { unGlobalStorage :: Storage }
  deriving (Eq, Show, Generic, NFData, Hash.Hashable)

instance Monoid GlobalStorage where
  mempty = GlobalStorage Map.empty
  (GlobalStorage m1) `mappend` (GlobalStorage m2) = GlobalStorage (m1 <> m2)

instance Pretty Key where
  ppr (Key key) = ppr key

newtype LocalStorage = LocalStorage { unLocalStorage :: Storage }
  deriving (Eq, Show, Generic, NFData, Hash.Hashable)

instance Monoid LocalStorage where
  mempty = LocalStorage Map.empty
  (LocalStorage m1) `mappend` (LocalStorage m2) = LocalStorage (m1 <> m2)

storageSize :: Storage -> Int
storageSize = Map.size

-- XXX
validateStorage :: Storage -> IO Bool
validateStorage storage = return True

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

decodeStorage :: ByteString -> Either [Char] Storage
decodeStorage = decode

encodeStorage :: Storage -> ByteString
encodeStorage = encode

decodeLocalStorage :: ByteString -> Either [Char] LocalStorage
decodeLocalStorage = decode

instance Serialize Key where
  put (Key bs) = S.put bs
  get = Key <$> S.get

instance Serialize GlobalStorage where
  put (GlobalStorage storage) = S.put storage
  get = GlobalStorage <$> S.get

instance Serialize LocalStorage where
  put (LocalStorage storage) = S.put storage
  get = LocalStorage <$> S.get

instance ToJSON GlobalStorage where
  toJSON = toJSON . unGlobalStorage

instance ToJSON LocalStorage where
  toJSON = toJSON . unLocalStorage

instance ToJSONKey Value where

instance ToJSON Value where
  toJSON = \case
     VInt n       -> object ["tag" .= ("VInt" :: Text), "contents" .= toJSON n]
     VFloat n     -> object ["tag" .= ("VFloat" :: Text), "contents" .= toJSON n]
     VFixed f     -> object ["tag" .= ("VFixed" :: Text), "contents" .= A.toJSON f]
     VBool n      -> object ["tag" .= ("VBool" :: Text), "contents" .= toJSON n]
     VVoid        -> object ["tag" .= ("VVoid" :: Text), "contents" .= A.Null]
     VSig sig     -> object ["tag" .= ("VSig" :: Text), "contents" .= A.toJSON sig]
     VMsg n       -> object ["tag" .= ("VMsg" :: Text), "contents" .= A.toJSON n]
     VAccount n   -> object ["tag" .= ("VAccount" :: Text), "contents" .= toJSON n]
     VAsset n     -> object ["tag" .= ("VAsset" :: Text), "contents" .= toJSON n]
     VContract n  -> object ["tag" .= ("VContract" :: Text), "contents" .= toJSON n]
     VDateTime n  -> object ["tag" .= ("VDateTime" :: Text), "contents" .= toJSON n]
     VTimeDelta n -> object ["tag" .= ("VTimeDelta" :: Text), "contents" .= toJSON n]
     VState n     -> object ["tag" .= ("VState" :: Text), "contents" .= toJSON n]
     VEnum c      -> object ["tag" .= ("VEnum" :: Text), "contents" .= toJSON c]
     VMap vmap    -> object ["tag" .= ("VMap" :: Text), "contents" .= toJSON vmap]
     VSet vset    -> object ["tag" .= ("VSet" :: Text), "contents" .= toJSON vset]
     VUndefined   -> object ["tag" .= ("VUndefined" :: Text), "contents" .= A.Null]

instance FromJSON GlobalStorage where
  parseJSON = fmap GlobalStorage . parseJSON

instance FromJSON LocalStorage where
  parseJSON = fmap LocalStorage . parseJSON

instance FromJSON Value where
  parseJSON v = case v of
    A.Array _  -> typeMismatch "Cannot parse array." v
    A.String _ -> typeMismatch "Please pass tagged objects, not json values" v
    A.Null     -> typeMismatch "Please pass tagged objects, not json values" v
    A.Bool _   -> typeMismatch "Please pass tagged objects, not json values" v
    A.Number _ -> typeMismatch "Please pass tagged objects, not json values" v
    A.Object o -> do
      constr :: Text <- o .: "tag"
      case constr of
        "VInt"      -> do
          c <- toBoundedInteger <$> (o .: "contents")
          case c of
            Just n  -> pure (VInt n)
            Nothing -> typeMismatch "Cannot parse unbounded integer." v
        "VFloat"      -> do
          c <- toBoundedRealFloat <$> (o .: "contents")
          case c of
            Right n  -> pure (VFloat n)
            Left _   -> typeMismatch "Cannot parse unbounded float." v
        "VBool"     -> VBool     <$> o .: "contents"
        "VAccount"  -> VAccount  <$> o .: "contents"
        "VAsset"    -> VAsset    <$> o .: "contents"
        "VContract" -> VContract <$> o .: "contents"
        "VDateTime"-> do
            c <- parseDatetime <$> (o .: "contents")
            case c of
              (Just dt) -> pure $ VDateTime $ DateTime dt
              Nothing -> typeMismatch "Invalid date format, expecting ISO8601, given:" v
        "VTimeDelta" -> VTimeDelta  <$> o .: "contents"
        "VSig"      -> VSig      <$> o .: "contents"
        "VFixed"    -> VFixed    <$> o .: "contents"
        "VMsg"      -> VMsg      <$> o .: "contents"
        "VEnum"     -> VEnum     <$> o .: "contents"
        "VVoid"     -> pure VVoid
        "VUndefined" -> pure VUndefined
        tag -> typeMismatch "Value tag as a string" v

instance ToJSONKey Key where
  toJSONKey = toJSONKeyText unKey

instance FromJSONKey Key where
  fromJSONKey = A.FromJSONKeyText Key

instance ToJSON Key where
  toJSON  = toJSON . unKey

instance FromJSON Key where
  parseJSON v =
    case v of
      A.String s -> pure  $ Key s
      _ -> typeMismatch "Key" v

-------------------------------------------------------------------------------

instance ToField Key where
  toField = toField . unKey

instance FromField Key where
  fromField f mdata = do
    bs <- fromField f mdata
    case bs of
      Nothing          -> returnError UnexpectedNull f ""
      Just (Left err)  -> returnError ConversionFailed f err
      Just (Right key) -> pure $ Key key

-------------------------------------------------------------------------------
-- Hashing
-------------------------------------------------------------------------------

instance Hash.Hashable Key where
  toHash (Key bs) = Hash.toHash bs

base16HashToInteger :: Hash.Hash Encoding.Base16ByteString -> Integer
base16HashToInteger = os2ip

hashStorage :: Storage -> Integer
hashStorage = base16HashToInteger . Hash.toHash
