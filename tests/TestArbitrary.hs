module TestArbitrary where

import qualified SafeInteger as SI
import qualified SafeString as SS

import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic

import qualified Data.Map as Map
import qualified Data.ByteString as BS
import Transaction
import Address
import Metadata
import qualified Encoding
import qualified Hash
import qualified Script
import qualified Fixed
import qualified Asset

import qualified Datetime.Types as DT
import qualified Datetime as DT
import qualified Data.Hourglass as DH
import qualified Data.Hourglass.Types as DH
import qualified Data.Time.Calendar as DC
import qualified Data.Text as T
import Protolude

-- TODO: Should we avoid orphan instances?
-- TODO: What is Fixed1 Fixed1

instance Arbitrary SI.SafeInteger where
  arbitrary =
    let minBound' = SI.fromSafeInteger minBound
        maxBound' = SI.fromSafeInteger maxBound
    in SI.toSafeInteger' <$> choose (minBound',maxBound')

newtype UnsafeInteger = UnsafeInteger Integer
  deriving Show

instance Arbitrary UnsafeInteger where
  arbitrary = UnsafeInteger <$> choose (n*2, n*(2^4096))
    where
      n = SI.fromSafeInteger maxBound

genByteString :: Gen BS.ByteString
genByteString =
  (toS :: [Char] -> BS.ByteString) <$>
  (arbitrary `suchThat` (\s -> length s < SS.maxSize))

instance Arbitrary SS.SafeString where
  arbitrary = SS.fromBytes' <$> genByteString

instance Arbitrary (Address a) where
  arbitrary = Address <$> arbitrary

instance (Encoding.ByteStringEncoding a) => Arbitrary (Hash.Hash a) where
  arbitrary = Hash.toHash <$> genByteString

instance Arbitrary DT.Datetime where
  arbitrary = DT.posixToDatetime <$> choose (1, 32503680000) -- (01/01/1970, 01/01/3000)

instance Arbitrary DT.Period where
  arbitrary = do
    year <- choose (0,1000)
    month <- choose (0,12)
    let monthNumDays = DC.gregorianMonthLength (fromIntegral year) (fromIntegral month)
    day <- choose (0, monthNumDays)
    pure $ DT.Period $ DH.Period year month day

instance Arbitrary DT.Duration where
  arbitrary = fmap DT.Duration $ DH.Duration
    <$> (fmap DH.Hours $ choose (0,23))
    <*> (fmap DH.Minutes $ choose (0,59))
    <*> (fmap DH.Seconds $ choose (0,59))
    <*> pure 0

instance Arbitrary DT.Delta where
  arbitrary = DT.Delta <$> arbitrary <*> arbitrary

instance Arbitrary Fixed.FixedN where
  arbitrary = Fixed.mkFixed <$> arbitrary <*> arbitrary

instance Arbitrary Fixed.PrecN where
  arbitrary = elements
    [ Fixed.Prec1
    , Fixed.Prec2
    , Fixed.Prec3
    , Fixed.Prec4
    , Fixed.Prec5
    , Fixed.Prec6
    ]

instance Arbitrary Script.Value where
  arbitrary = oneof
    [ Script.VInt <$> arbitrary
    , Script.VFloat <$> arbitrary
    , Script.VFixed <$> arbitrary
    , Script.VBool <$> arbitrary
    , Script.VAccount <$> arbitrary
    , Script.VAsset <$> arbitrary
    , Script.VMsg <$> arbitrary
    , Script.VSig <$> arbitrary
    , pure Script.VVoid
    , Script.VDateTime <$> arbitrary
    , Script.VTimeDelta <$> arbitrary
    , Script.VEnum <$> arbitrary
    , Script.VState <$> arbitrary
    , pure Script.VUndefined
    ]

instance Arbitrary Script.DateTime where
  arbitrary = Script.DateTime <$> arbitrary

instance Arbitrary Script.TimeDelta where
  arbitrary = Script.TimeDelta <$> arbitrary

instance Arbitrary Script.EnumConstr where
  arbitrary = Script.EnumConstr <$> arbitrary

instance Arbitrary Script.Label where
  arbitrary = Script.Label <$> arbitrary

instance Arbitrary T.Text where
  arbitrary = (toS :: [Char] -> T.Text) <$> arbitrary

instance Arbitrary TxContract where
  arbitrary = oneof
    [ CreateContract <$> arbitrary
    -- , SyncLocal
    , Call <$> arbitrary <*> genByteString <*> arbitrary
    ]

instance Arbitrary Asset.Ref where
  arbitrary = elements
    [ Asset.USD
    , Asset.GBP
    , Asset.EUR
    , Asset.CHF
    , Asset.Token
    , Asset.Security
    ]

instance Arbitrary Asset.AssetType where
  arbitrary = oneof
    [ pure Asset.Discrete
    -- , Asset.Fractional <$> arbitrary
    , pure Asset.Binary
    ]

instance Arbitrary Metadata where
  arbitrary = Metadata <$> arbitrary

instance Arbitrary Asset.Holder where
  arbitrary = oneof
    [ Asset.Holder <$> (arbitrary :: Gen (Address AAccount))
    -- Warning: See Serialize Holder on Asset.hs
    -- Holder serializer defaults to Holder (Address AAccount)
    -- TODO: Fix it in the future and uncomment the following line
    -- , Asset.Holder <$> (arbitrary :: Gen (Address AContract))
    ]

instance Arbitrary TxAsset where
  arbitrary = oneof
    [ CreateAsset <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , Transfer <$> arbitrary <*> arbitrary <*> arbitrary
    , Circulate <$> arbitrary <*> arbitrary
    , Bind <$> arbitrary <*> arbitrary <*> arbitrary
    , RevokeAsset <$> arbitrary
    ]

instance Arbitrary TxAccount where
  arbitrary = oneof
    [ CreateAccount <$> arbitrary <*> arbitrary <*> arbitrary
    , RevokeAccount <$> arbitrary
    ]

instance Arbitrary TransactionHeader where
  arbitrary = oneof
    [ TxContract <$> arbitrary
    , TxAsset <$> arbitrary
    , TxAccount <$> arbitrary
    ]
