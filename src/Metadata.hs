{-|

Metadata data structures and serialization.

--}


{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}


module Metadata (
  Metadata(..),
  parseMetadata
) where

import Protolude
import qualified Hash
import qualified Utils

import qualified Data.Map as Map
import qualified Data.Aeson as A
import qualified Data.Serialize as S
import qualified Data.ByteString as BS

import Database.PostgreSQL.Simple.ToField   (ToField(..), Action(..))
import Database.PostgreSQL.Simple.FromField (FromField(..), ResultError(..), returnError)


-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

newtype Metadata = Metadata
  { unMetadata :: Map ByteString ByteString }
  deriving (Show, Eq, Generic, NFData, Hash.Hashable)

instance Monoid Metadata where
  mempty = Metadata mempty
  (Metadata m1) `mappend` (Metadata m2) =
    Metadata $ m1 <> m2

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

instance S.Serialize Metadata where
  put (Metadata m) = do
    let len = Map.size m
    S.putWord16be $ Utils.toWord16 len
    go $ sortBy (\a b -> compare (fst a) (fst b)) $ Map.toList m

    where
      go [] = return ()
      go ((k,v):xs) = do
        S.putWord16be $ Utils.toWord16 $ BS.length k
        S.putByteString k
        S.putWord16be $ Utils.toWord16 $ BS.length v
        S.putByteString v
        go xs
  get = do
      len <- S.getWord16be
      go [] 0 len
    where
      go acc i len
        | i == len = return $ Metadata $ Map.fromList acc
        | otherwise = do
          keyLen <- S.getWord16be
          key    <- S.getBytes $ Utils.toInt keyLen
          valLen <- S.getWord16be
          val    <- S.getBytes $ Utils.toInt valLen
          go ((key, val) : acc) (i+1) len

instance A.ToJSON Metadata where
  toJSON (Metadata metadata) = A.toJSON $
    map decodeUtf8 $ Map.mapKeys decodeUtf8 metadata

instance A.FromJSON Metadata where
  parseJSON = fmap parseMetadata . A.parseJSON

parseMetadata :: Map Text Text -> Metadata
parseMetadata keys = Metadata $ map encodeUtf8 (Map.mapKeys encodeUtf8 $ keys)

-------------------------------------------------------------------------------
-- Postgres DB
-------------------------------------------------------------------------------

instance ToField Metadata where
  toField = EscapeByteA . S.encode . Map.toList . unMetadata

instance FromField Metadata where
  fromField f mdata = do
    bs <- fromField f mdata
    case fmap Map.fromList . S.decode <$> bs of
      Nothing               -> returnError UnexpectedNull f ""
      Just (Left err)       -> returnError ConversionFailed f err
      Just (Right metadata) -> return $ Metadata metadata
