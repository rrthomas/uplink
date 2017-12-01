{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DB.PostgreSQL.Asset (

  AssetRow(..),
  assetToRowTypes,
  rowTypesToAsset,

  queryAsset,
  queryAssets,

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
  , assetReference     :: Maybe Int16
  , assetType          :: Int16
  , assetTypePrec      :: Maybe Int16
  , assetAddress       :: Address
  } deriving (Generic)

instance ToRow AssetRow
instance FromRow AssetRow

data HoldingsRow = HoldingsRow
  { holdingsAsset   :: Address
  , holdingsHolder  :: Address
  , holdingsBalance :: Int64
  } deriving (Generic)

instance ToRow HoldingsRow
instance FromRow HoldingsRow

assetToRowTypes :: Asset -> (AssetRow, [HoldingsRow])
assetToRowTypes Asset{..} = (assetRow, holdingsRows)
  where
    (atyp :: Int16, atypPrec) =
      case assetType of
        Asset.Discrete     -> (0,Nothing)
        Asset.Fractional n -> (1,Just $ fromIntegral n)
        Asset.Binary       -> (2,Nothing)

    ref = fromIntegral . fromEnum <$> reference

    assetRow = AssetRow
      { assetName      = toS name
      , assetIssuer    = issuer
      , assetIssuedOn  = issuedOn
      , assetSupply    = supply
      , assetReference = ref
      , assetType      = atyp
      , assetTypePrec  = atypPrec
      , assetAddress   = address
      }

    holdingsRows =
      map (uncurry $ HoldingsRow address) $
        Map.toList (Asset.unHoldings holdings)

rowTypesToAsset :: (AssetRow, [HoldingsRow]) -> Either Text Asset
rowTypesToAsset (assetRow, holdingsRows) =
    case eAssetType of
      Left err         -> Left err
      Right assetType' -> Right $ Asset
        { name      = toS $ assetName assetRow
        , issuer    = assetIssuer assetRow
        , issuedOn  = assetIssuedOn assetRow
        , supply    = assetSupply assetRow
        , reference = assetRef
        , holdings  = holdings'
        , assetType = assetType'
        , address   = assetAddress assetRow
        }
  where
    holdings' = Asset.Holdings $ Map.fromList $
      map holdingsRowToHoldingEntry holdingsRows

    holdingsRowToHoldingEntry hrow =
      (holdingsHolder hrow, holdingsBalance hrow)

    -- Convert AssetRow assetType and assetTypePrec to Asset assetType
    eAssetType = do
      let rowAssetType = DB.PostgreSQL.Asset.assetType assetRow
      case (rowAssetType, assetTypePrec assetRow) of
        (0,Nothing) -> Right $ Asset.Discrete
        (1,Just n)  -> Right $ Asset.Fractional $ fromIntegral n
        (2,Nothing) -> Right $ Asset.Binary
        otherwise   -> Left $
          "Could not parse `assetType` field of Asset from DB"

    -- Convert AssetRow ref to Asset ref
    assetRef =
      fmap (toEnum . fromIntegral) $
        assetReference assetRow

--------------------------------------------------------------------------------
-- Queries (SELECTs)
--------------------------------------------------------------------------------

queryAsset :: Connection -> Address -> IO (Either Text Asset)
queryAsset conn assetAddr = do
  mAssetRow <- queryAssetRow conn assetAddr
  case mAssetRow of
    Nothing -> pure $ Left $ Text.intercalate " "
      [ "PostgreSQL: Asset with addr"
      , toS (rawAddr assetAddr)
      , "does not exist."
      ]
    Just assetRow -> do
      holdingsRows <- queryHoldingsRows conn assetAddr
      pure $ rowTypesToAsset (assetRow, holdingsRows)

queryAssets :: Connection -> IO (Either Text [Asset])
queryAssets conn = do
  assetRows <- queryAssetRows conn
  fmap sequence $
    forM assetRows $ \assetRow -> do
      holdingsRows <- queryHoldingsRows conn (assetAddress assetRow)
      pure $ rowTypesToAsset (assetRow, holdingsRows)

--------------------------------------------------------------------------------

queryAssetRow :: Connection -> Address -> IO (Maybe AssetRow)
queryAssetRow conn assetAddr = headMay <$>
  query conn "SELECT * FROM assets where address=?" (Only assetAddr)

queryAssetRows :: Connection -> IO [AssetRow]
queryAssetRows conn =
  query_ conn "SELECT * FROM assets"

queryHoldingsRows :: Connection -> Address -> IO [HoldingsRow]
queryHoldingsRows conn assetAddr =
  query conn "SELECT asset,holder,balance FROM holdings WHERE asset=?" (Only assetAddr)

--------------------------------------------------------------------------------
-- Inserts
--------------------------------------------------------------------------------

insertAsset :: Connection -> Asset -> IO ()
insertAsset conn asset = do
    insertAssetRow conn assetRow
    insertHoldingsRows conn holdingsRows
  where
    (assetRow, holdingsRows) = assetToRowTypes asset

insertAssets :: Connection -> [Asset] -> IO ()
insertAssets conn assets =
  forM_ assets (insertAsset conn)

insertAssetRow :: Connection -> AssetRow -> IO ()
insertAssetRow conn assetRow = void $
  execute conn "INSERT INTO assets VALUES (?,?,?,?,?,?,?,?)" assetRow

insertHoldingsRows :: Connection -> [HoldingsRow] -> IO ()
insertHoldingsRows conn holdingsRows = void $
  executeMany conn "INSERT INTO holdings (asset,holder,balance) VALUES (?,?,?)" holdingsRows

--------------------------------------------------------------------------------
-- Deletes
--------------------------------------------------------------------------------

deleteAsset :: Connection -> Address -> IO ()
deleteAsset conn addr = void $ do
  execute conn "DELETE FROM assets WHERE address=?" (Only addr)
  execute conn "DELETE FROM holdings WHERE asset=?" (Only addr)

deleteAssets :: Connection -> IO ()
deleteAssets conn = void $
  execute_ conn "DELETE FROM assets"
