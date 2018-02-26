{-|

Asset data types.

-}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Asset (
  -- ** Assets
  Asset(..),
  AssetError(..),
  AssetType(..),

  Ref(..),
  validateAsset,

  -- ** Creation
  createAsset,

  -- ** Balances
  Balance,
  balance,
  displayType,
  maxBalance,
  maxDisplay,
  preallocate,

  -- ** Holdings
  Holder,
  Holdings(..),
  emptyHoldings,
  transferHoldings,
  circulateSupply,

  -- ** Serialization
  encodeAsset,
  decodeAsset,
  putAssetType,
  getAssetType,
  putRef,
  getRef,

  -- ** Save/Load Asset
  saveAsset,
  loadAsset,

) where

import Protolude hiding (put, get, putByteString)

import Time (Timestamp)
import Address (Address, rawAddr, fromRaw)
import qualified Key
import qualified Metadata
import qualified Hash
import qualified Time
import qualified Fixed
import qualified Utils
import qualified Address

import Control.Monad (fail)
import Data.Aeson (ToJSON(..), FromJSON, (.=), (.:), (.:?) , object)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (typeMismatch, Parser)
import Data.Serialize

import qualified Data.Map as Map
import qualified Data.Aeson as A
import qualified Data.ByteArray as B
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson.Encode.Pretty as A

import Database.PostgreSQL.Simple.ToRow     (ToRow(..))
import Database.PostgreSQL.Simple.FromRow   (FromRow(..), field)
import Database.PostgreSQL.Simple.ToField   (ToField(..))
import Database.PostgreSQL.Simple.FromField (FromField(..), ResultError(..), returnError)

import Numeric (showFFloat)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | A holder of a balance in an asset.
type Holder = Address

-- | A quantity of units of value in an asset.  The smallest non-zero amount
-- unit is 0.0000001 represented as an integer value of one. The largest amount
-- unit possible is ((2^63)-1)/(10^7) (derived from max int64 scaled down) which
-- is 922,337,203,685.4775807.
type Balance = Int64

-- | A map of holdings to balances. i.e. a ledger
newtype Holdings = Holdings { unHoldings :: Map.Map Holder Balance }
  deriving (Eq, Ord, Show, Generic, NFData, Serialize)

instance ToJSON Holdings where
  toJSON (Holdings holdings) = toJSON holdings

instance FromJSON Holdings where
  parseJSON = fmap Holdings . A.parseJSON

instance Monoid Holdings where
  mempty = Holdings mempty
  (Holdings h1) `mappend` (Holdings h2) =
    Holdings $ h1 <> h2

-- | An asset is a named quantity that once issued is a fixed supply of
-- immutably issued "units" of value. Units can be held by other addresses.
data Asset = Asset
  { name      :: ByteString -- ^ Name of asset
  , issuer    :: Holder     -- ^ Issuer
  , issuedOn  :: Timestamp  -- ^ Timestamp
  , supply    :: Balance    -- ^ Total supply
  , holdings  :: Holdings   -- ^ Holdings map
  , reference :: Maybe Ref  -- ^ Reference unit
  , assetType :: AssetType  -- ^ Asset type
  , address   :: Address    -- ^ Asset address
  , metadata  :: Metadata.Metadata   -- ^ Asset address
  } deriving (Eq, Show, Generic, NFData, Serialize)

-- | An asset reference is metadata assigning a off-chain reference quantity to
-- a single unit of an on-chain asset.
data Ref
  = USD               -- ^ US Dollars
  | GBP               -- ^ British Pounds
  | EUR               -- ^ Euros
  | CHF               -- ^ Swiss Francs
  | Token             -- ^ Abstract token
  | Security          -- ^ Security
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic, NFData, ToJSON, FromJSON, Hash.Hashable)

instance ToField Ref where
  toField = toField . (show :: Ref -> [Char])

instance FromField Ref where
  fromField f mdata = do
    bs <- fromField f mdata
    case (readMaybe :: [Char] -> Maybe Ref) . toS <$> (bs :: Maybe ByteString) of
      Nothing               -> returnError UnexpectedNull f ""
      Just Nothing          -> returnError ConversionFailed f "Failed to read Ref from field."
      Just (Just assetType) -> return assetType

-- | Type of an asset's value. Underlying value is always a Int64, but this
-- informs the representation and range of valid values.
data AssetType
  = Discrete               -- ^ Discrete (Non-zero integer value)
  | Fractional Fixed.PrecN -- ^ Fractional (Fixed point decimal value)
  | Binary                 -- ^ Binary (Held/Not-Held) (supply is +1 for held, 0 for not-held)
  deriving (Eq, Ord, Show, Read, Generic, NFData, Hashable, Hash.Hashable)

instance ToField AssetType where
  toField = toField . (show :: AssetType -> [Char])

instance FromField AssetType where
  fromField f mdata = do
    bs <- fromField f mdata
    case (readMaybe :: [Char] -> Maybe AssetType) . toS <$> (bs :: Maybe ByteString) of
      Nothing               -> returnError UnexpectedNull f ""
      Just Nothing          -> returnError ConversionFailed f "Failed to read AssetType from field."
      Just (Just assetType) -> return assetType

-- | Initial holdings, all allocated to issuer.
emptyHoldings :: Holdings
emptyHoldings = mempty

-- | Verify that an asset value contains valid data
validateAsset :: Asset -> IO Bool
validateAsset Asset{..} = do
    return $ and [ totalHoldings <= supply ]
  where
    totalHoldings = foldl (+) 0 $ unHoldings holdings

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

instance ToJSON Asset where
  toJSON asset = object
    [ "name"      .= decodeUtf8 (name asset)
    , "issuer"    .= issuer asset
    , "issuedOn"  .= issuedOn asset
    , "supply"    .= supply asset
    , "holdings"  .= holdings asset
    , "reference" .= reference asset
    , "assetType" .= assetType asset
    , "address"   .= address asset
    , "metadata"  .= metadata asset
    ]

instance FromJSON Asset where
  parseJSON (A.Object v) = do
    name      <- encodeUtf8 <$> v .: "name"
    issuer    <- v .: "issuer"
    issuedOn  <- v .: "issuedOn"
    supply    <- v .: "supply"
    holdings  <- v .: "holdings"
    reference <- v .: "reference"
    assetType <- v .: "assetType"
    address   <- v .: "address"
    metadata  <- v .: "metadata"
    return Asset{..}

  parseJSON invalid = typeMismatch "Asset" invalid

instance Binary.Binary Asset where
  put tx = Binary.put $ encode tx
  get = do
    bs <- Binary.get
    case decode bs of
      (Right tx) -> return tx
      (Left err) -> fail err

-------------------------------------------------------------------------------

instance Serialize Ref where
  put = putRef
  get = getRef

putRef :: Ref -> PutM ()
putRef USD = putWord16be 3 >> putByteString "USD"
putRef EUR = putWord16be 3 >> putByteString "EUR"
putRef GBP = putWord16be 3 >> putByteString "GBP"
putRef CHF = putWord16be 3 >> putByteString "CHF"
putRef Token = putWord16be 5 >> putByteString "Token"
putRef Security = putWord16be 8 >> putByteString "Security"

getRef :: Get Ref
getRef = do
  len <- getWord16be
  str <- getBytes (Utils.toInt len)
  if | str == "GBP" -> pure GBP
     | str == "EUR" -> pure EUR
     | str == "CHF" -> pure CHF
     | str == "USD" -> pure USD
     | str == "Token" -> pure Token
     | str == "Security" -> pure Security
     | otherwise -> fail $ "Cannot decode asset reference: " <> toS str

-------------------------------------------------------------------------------

instance ToJSON AssetType where
  toJSON (Fractional prec) = object
    ["tag" .= ("Fractional" :: Text), "contents" .= (fromEnum prec + 1)]
  toJSON Discrete = object
    ["tag" .= ("Discrete" :: Text), "contents" .= A.Null]
  toJSON Binary = object
    ["tag" .= ("Binary" :: Text), "contents" .= A.Null]

instance FromJSON AssetType where
  parseJSON (A.Object v) = do
    constr <- v .: "tag"
    case constr :: [Char] of
      "Discrete"   -> pure Discrete
      "Binary"     -> pure Binary
      "Fractional" -> do
        prec <- v .: "contents"
        pure $ Fractional prec
      invalid -> fail $ invalid ++ " is not a valid AssetType"

  parseJSON invalid = typeMismatch "AssetType" invalid

instance Serialize AssetType where
  put = putAssetType
  get = getAssetType

getAssetType :: Get AssetType
getAssetType = do
  len <- getWord16be
  str <- getBytes (Utils.toInt len)
  if | str == "Discrete"   -> pure Discrete
     | str == "Binary"     -> pure Binary
     | str == "Fractional" -> Fractional <$> get
     | otherwise -> fail $ "Cannot decode asset type : " <> toS str

putAssetType :: AssetType -> PutM ()
putAssetType Discrete = putWord16be 8 >> putByteString "Discrete"
putAssetType Binary   = putWord16be 6 >> putByteString "Binary"
putAssetType (Fractional prec) = do
    putWord16be 10
    putByteString "Fractional"
    put prec

-------------------------------------------------------------------------------
-- Printing
-------------------------------------------------------------------------------

-- | 9223372036854775807
maxBalance :: Int64
maxBalance = (2^63)-1

-- | 0.0000001
minBalance :: Int64
minBalance = 10^7

-- | 922,337,203,685.4775807
maxDisplay :: Double
maxDisplay = fromIntegral maxBalance / fromIntegral minBalance

-- | Display asset type balance as text
displayType :: AssetType -> Balance -> Text
displayType ty bal = case ty of
  Discrete        -> show bal
  Binary          -> if bal > 0 then "held" else "not-held"
  Fractional prec -> toS $ showFFloat (Just $ (+1) $ fromEnum prec) (fromIntegral bal) ""

-------------------------------------------------------------------------------
-- Operations over Assets
-------------------------------------------------------------------------------

data AssetError
  = InsufficientHoldings Holder Balance
  | InsufficientSupply Address Balance     -- [Char] for serialize instance
  | CirculatorIsNotIssuer Address Address
  | SelfTransfer Address
  | HolderDoesNotExist Address
  deriving (Show, Eq, Generic, Serialize)

-- | Binary serialize an asset.
encodeAsset :: Asset -> ByteString
encodeAsset = encode

-- | Binary deserialize an asset.
decodeAsset :: ByteString -> Either Text Asset
decodeAsset = first toS . decode

-- | Lookup balance of a holder, returning Nothing if no
balance :: Asset -> Holder -> Maybe Balance
balance asset holder = Map.lookup holder (unHoldings $ holdings asset)

-- | Amount of assets in circulation.
circulation :: Asset -> Balance
circulation asset = supply asset - inCirculation
  where
    inCirculation = foldl (+) 0 $ unHoldings (holdings asset)

-- | Set an assets initial balances.
preallocate :: [(Holder, Balance)] -> Asset -> Asset
preallocate balances asset = asset { holdings = holdings' }
  where
    holdings' = Holdings $ Map.fromList balances

-- | Transfer an amount of the asset supply to an account's holdings
circulateSupply :: Address -> Balance -> Asset -> Either AssetError Asset
circulateSupply addr bal asset
  | integrity = Right $ asset { holdings = holdings', supply = supply' }
  | otherwise = Left $ InsufficientSupply (address asset) (supply asset)
  where
    holdings' = Holdings $ clearZeroes $
      Map.insertWith (+) addr bal $ unHoldings (holdings asset)

    supply'   = supply asset - bal
    integrity = supply asset >= bal

    clearZeroes = Map.filter (/= 0)

-- | Atomically transfer holdings 'from' one account 'to' another
transferHoldings :: Address -> Address -> Balance -> Asset -> Either AssetError Asset
transferHoldings from to amount asset
  | from == to = Left $ SelfTransfer from
  | otherwise  =
      case balance asset from of
        Nothing ->
          Left $ HolderDoesNotExist from
        Just bal
          | amount <= bal -> do
              asset' <- circulateSupply from (negate amount) asset
              circulateSupply to amount asset'
          | otherwise     ->
              Left $ InsufficientHoldings from bal
  where
    assetIssuer = issuer asset

-- | Smart constructor for asset
createAsset
  :: ByteString
  -> Holder
  -> Balance
  -> Maybe Ref
  -> AssetType
  -> Time.Timestamp
  -> Address
  -> Metadata.Metadata
  -> Asset
createAsset name holder supply mRef assetType ts addr metadata =
  Asset name holder ts supply mempty mRef assetType addr metadata

-------------------------------------------------------------------------------
-- Export / Import
-------------------------------------------------------------------------------

-- | Load asset from JSON
loadAsset :: FilePath -> IO (Either [Char] Asset)
loadAsset fp = fmap Aeson.eitherDecodeStrict (BS.readFile fp)

-- | Save asset as JSON
saveAsset :: Asset -> FilePath -> IO ()
saveAsset asset fp = BSL.writeFile fp $ A.encodePretty asset
