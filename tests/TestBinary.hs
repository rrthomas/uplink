{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TestBinary (
  binaryTests,
) where

import Protolude

import Data.ByteString
import Data.Serialize as S
import Data.Binary as B

import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic

import Fixed
import qualified SafeInteger as SI
import qualified SafeString as SS
import qualified Key
import qualified Block
import qualified Hash
import qualified Account
import qualified Address
import qualified Script
import qualified Time
import qualified Consensus.Authority as CA
import qualified Consensus.Authority.Params as CAP

import Network.P2P.Consensus as NPC
import qualified Reference as Ref

import Crypto.Number.Basic

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

-- | Test that a data structure round-trips throught its binary serializer and
-- deserializer preserving the same structure.
binaryTests :: TestTree
binaryTests =
  testGroup "Binary Tests" $
    [ testCase "Block Serialization" $ do
        let testGenBlockSeed = "83bc234a"
        genBlock <- Block.genesisBlock testGenBlockSeed Ref.testTimestamp Ref.testPoA
        binaryTest Block.encodeBlock Block.decodeBlock (Ref.testBlock genBlock [])

    , testCase "Address Serialization" $ do
        binaryTest S.encode S.decode (pure Ref.testAddr)

    , testCase "Transaction binary roundtrip (uses Serialize instance)" $ do
        let tx = Ref.testTx Ref.testCall
        binaryTest (toS . B.encode) (Right . B.decode . toSL) (pure tx)
    , testCase "Account binary roundtrip (uses Serialize instance)" $
        binaryTest (toS . B.encode) (Right . B.decode . toSL) (pure Ref.testAccount)
    , testCase "Asset binary roundtrip (uses Serialize instance)" $
        binaryTest (toS . B.encode) (Right . B.decode . toSL) (pure Ref.testAsset1)
    , testCase "Contract binary roundtrip (uses Serialize instance)" $
        binaryTest (toS . B.encode) (Right . B.decode . toSL) (pure $ Ref.testContract Ref.testTimestamp)

    , testCase "Account Serialization" $ do
        (acct, _) <- Account.newAccount mempty mempty
        binaryTest Account.encodeAccount Account.decodeAccount (pure acct)

    , localOption (QuickCheckTests 10000) $
        testProperty "decode(encode(SafeString)) == SafeString)" $ \(str :: [Char]) ->
          let bs = toS str
              ss = SS.fromBytes bs
              res = S.decode $ S.encode ss
          in if Data.ByteString.length bs > SS.maxSize
               then isLeft res
               else Right ss == res

    , localOption (QuickCheckTests 10000) $
        testProperty "decode(encode(SafeInteger n)) == SafeInteger n" $ \(safeInt :: SI.SafeInteger) ->
          S.decode (S.encode safeInt) == Right safeInt

    , testProperty "Safe deserialization of malicious SafeInteger" $ \(UnsafeInteger n) ->
        let safeInt' = SI.unsafeToSafeInteger n
        in case S.decode (SI.unsafeEncode safeInt') of -- serialize & deserialize a safe int larger than maxBits
          Left err -> True -- probably won't happen, but success if it does
          Right safeInt  ->
            -- resulting SafeInteger should be at max 4096 bits,
            -- shouldn't deserialize more bits than that.
            let n' = SI.fromSafeInteger safeInt
            in numBits n' <= SI.maxBits

    , testProperty "ECDSA.Signature Serialization" $ \(Positive n) ->
        monadicIO $ do
          let (pubKey, privKey) = Key.new' n
          blockHash <- run $ Ref.testBlockHash
          sig <- run $ Key.sign privKey blockHash
          assert $ Right sig == Key.decodeSig (Key.encodeSig sig)

    , testProperty "BlockSignature Serialization" $ \(Positive n) ->
        monadicIO $ do
          let (pubKey, privKey) = Key.new' n
          let addr = Address.deriveAddress pubKey
          blockHash <- run $ Ref.testBlockHash
          sig <- run $ Key.sign privKey blockHash
          let blockSig = Block.BlockSignature sig addr
          assert $ Right blockSig == S.decode (S.encode blockSig)

    , testCase "BlockSigMsg Serialization" $
        binaryTest S.encode S.decode $ do
          (pk,sk) <- Key.new
          let addr = Address.deriveAddress pk
          blockHash <- Ref.testBlockHash
          sig <- Key.sign sk blockHash
          let blockSig = Block.BlockSignature sig addr
          return $ NPC.BlockSigMsg blockSig

    , testProperty "FixedN Serialization" $ \(n' :: SI.SafeInteger) ->
        let n = SI.fromSafeInteger n'
            serializeTest f = Right f == S.decode (S.encode f)
            f1 = mkFixed Prec1 n
            f2 = mkFixed Prec2 n
            f3 = mkFixed Prec3 n
            f4 = mkFixed Prec4 n
            f5 = mkFixed Prec5 n
            f6 = mkFixed Prec6 n
        in and [ serializeTest f1
               , serializeTest f2
               , serializeTest f3
               , serializeTest f4
               , serializeTest f5
               , serializeTest f6
               ]

    ] ++ consensusMsgBinTests

consensusMsgBinTests :: [TestTree]
consensusMsgBinTests =
  [ testCase "Data.Binary: SignBlockMsg de/serialization" $
      binaryTest (toS . B.encode) (Right . B.decode . toSL) Ref.testSignBlockMsg
  , testCase "Data.Binary: BlockSigMsg de/serialization" $
      binaryTest (toS . B.encode) (Right . B.decode . toSL) Ref.testBlockSigMsg
  ]

type Encoder a = a -> ByteString
type Decoder a = ByteString -> Either [Char] a

binaryTest
  :: (Eq a, Show a)
  => Encoder a
  -> Decoder a
  -> IO a
  -> Assertion
binaryTest f g gen = gen >>= \x -> Right x @=? (g (f x))
