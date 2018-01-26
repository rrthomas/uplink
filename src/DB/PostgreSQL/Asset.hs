{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module DB.PostgreSQL.Asset (

  AssetRow(..),
  HoldingsRow(..),

  assetToRowTypes,
  rowTypesToAsset,

  queryAsset,
  queryAssets,
  queryAssetsByAddrs,

  assetRowToAsset,
  assetRowsToAssets,

  insertAsset,
  insertAssets,

  deleteAsset,
  deleteAssets,
) where

import Protolude

import Control.Monad (fail)

import Data.Int
import qualified Data.Map as Map
import qualified Data.Text as Text

import Asset (Asset(..))
import Address
import qualified Asset
import qualified Time

import DB.PostgreSQL.Error

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField

--------------------------------------------------------------------------------
-- Types and Conversions
--------------------------------------------------------------------------------

data AssetRow = AssetRow
  { assetName          :: Text
  , assetIssuer        :: Address
  , assetIssuedOn      :: Int64
  , assetSupply        :: Int64
  , assetReference     :: Maybe Asset.Ref
  , assetType          :: Asset.AssetType
  , assetAddress       :: Address
  } deriving (Show, Generic)

instance ToRow AssetRow
instance FromRow AssetRow

data HoldingsRow = HoldingsRow
  { holdingsAsset   :: Address
  , holdingsHolder  :: Address
  , holdingsBalance :: Int64
  } deriving (Show, Generic)

instance ToRow HoldingsRow
instance FromRow HoldingsRow

assetToRowTypes :: Asset -> (AssetRow, [HoldingsRow])
assetToRowTypes Asset{..} = (assetRow, holdingsRows)
  where
    assetRow = AssetRow
      { assetName      = toS name
      , assetIssuer    = issuer
      , assetIssuedOn  = issuedOn
      , assetSupply    = supply
      , assetReference = reference
      , assetType      = assetType
      , assetAddress   = address
      }

    holdingsRows =
      map (uncurry $ HoldingsRow address) $
        Map.toList (Asset.unHoldings holdings)

rowTypesToAsset :: (AssetRow, [HoldingsRow]) -> Either PostgreSQLError Asset
rowTypesToAsset (assetRow, holdingsRows) =
    Right $ Asset
      { name      = toS $ assetName assetRow
      , issuer    = assetIssuer assetRow
      , issuedOn  = assetIssuedOn assetRow
      , supply    = assetSupply assetRow
      , reference = assetReference assetRow
      , holdings  = holdings'
      , assetType = DB.PostgreSQL.Asset.assetType assetRow
      , address   = assetAddress assetRow
      }
  where
    holdings' = Asset.Holdings $ Map.fromList $
      map holdingsRowToHoldingEntry holdingsRows

    holdingsRowToHoldingEntry hrow =
      (holdingsHolder hrow, holdingsBalance hrow)

--------------------------------------------------------------------------------
-- Queries (SELECTs)
--------------------------------------------------------------------------------

queryAsset :: Connection -> Address -> IO (Either PostgreSQLError Asset)
queryAsset conn assetAddr = do
  mAssetRow <- queryAssetRow conn assetAddr
  case mAssetRow of
    Left err              -> pure $ Left err
    Right Nothing         -> pure $ Left $ AssetDoesNotExist assetAddr
    Right (Just assetRow) -> assetRowToAsset conn assetRow

queryAssets :: Connection -> IO (Either PostgreSQLError [Asset])
queryAssets conn = do
  eAssetRows <- queryAssetRows conn
  case eAssetRows of
    Left err -> pure $ Left err
    Right assetRows ->
      fmap sequence $
        forM assetRows $
          assetRowToAsset conn

queryAssetsByAddrs
  :: Connection
  -> [Address]
  -> IO (Either PostgreSQLError [Asset])
queryAssetsByAddrs conn addrs = do
  eAssetRows <- queryAssetRowsByAddrs conn addrs
  either (pure . Left) (assetRowsToAssets conn) eAssetRows

assetRowToAsset :: Connection -> AssetRow -> IO (Either PostgreSQLError Asset)
assetRowToAsset conn assetRow = do
  eHoldingsRows <- queryHoldingsRows conn (assetAddress assetRow)
  pure $ rowTypesToAsset . (assetRow,) =<< eHoldingsRows

assetRowsToAssets
  :: Connection
  -> [AssetRow]
  -> IO (Either PostgreSQLError [Asset])
assetRowsToAssets conn =
  fmap sequence . mapM (assetRowToAsset conn)

--------------------------------------------------------------------------------

queryAssetRow :: Connection -> Address -> IO (Either PostgreSQLError (Maybe AssetRow))
queryAssetRow conn assetAddr = fmap headMay <$>
  querySafe conn "SELECT * FROM assets WHERE address=?" (Only assetAddr)

queryAssetRows :: Connection -> IO (Either PostgreSQLError [AssetRow])
queryAssetRows conn =
  querySafe_ conn "SELECT * FROM assets"

queryAssetRowsByAddrs :: Connection -> [Address] -> IO (Either PostgreSQLError [AssetRow])
queryAssetRowsByAddrs conn addrs =
  querySafe conn "SELECT * from assets WHERE address IN ?" $
    Only $ In addrs

queryHoldingsRows :: Connection -> Address -> IO (Either PostgreSQLError [HoldingsRow])
queryHoldingsRows conn assetAddr =
  querySafe conn "SELECT * FROM holdings WHERE asset=?" (Only assetAddr)

--------------------------------------------------------------------------------
-- Inserts
--------------------------------------------------------------------------------

insertAsset :: Connection -> Asset -> IO (Either PostgreSQLError Int64)
insertAsset conn asset = do
    insertAssetRow conn assetRow
    insertHoldingsRows conn holdingsRows
  where
    (assetRow, holdingsRows) = assetToRowTypes asset

insertAssets :: Connection -> [Asset] -> IO (Either PostgreSQLError [Int64])
insertAssets conn assets =
  sequence <$> mapM (insertAsset conn) assets

insertAssetRow :: Connection -> AssetRow -> IO (Either PostgreSQLError Int64)
insertAssetRow conn assetRow =
  executeSafe conn "INSERT INTO assets VALUES (?,?,?,?,?,?,?)" assetRow

insertHoldingsRows :: Connection -> [HoldingsRow] -> IO (Either PostgreSQLError Int64)
insertHoldingsRows conn holdingsRows =
  executeManySafe conn "INSERT INTO holdings VALUES (?,?,?)" holdingsRows

--------------------------------------------------------------------------------
-- Deletes
--------------------------------------------------------------------------------

deleteAsset :: Connection -> Address -> IO (Either PostgreSQLError Int64)
deleteAsset conn addr = do
  _ <- executeSafe conn "DELETE FROM assets WHERE address=?" (Only addr)
  executeSafe conn "DELETE FROM holdings WHERE asset=?" (Only addr)

deleteAssets :: Connection -> IO (Either PostgreSQLError Int64)
deleteAssets conn =
  executeSafe_ conn "DELETE FROM assets"
