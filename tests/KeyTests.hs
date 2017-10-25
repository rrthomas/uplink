{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module KeyTests (
  keyTests,
) where

import Protolude

import Test.Tasty
import Test.QuickCheck.Monadic
import qualified Test.Tasty.QuickCheck as QC

import Crypto.PubKey.ECC.Generate (generateQ)

import qualified Key
import qualified Address

instance QC.Arbitrary Key.PubKey where
  arbitrary = do
    (QC.Positive d) <- QC.arbitrary
    return $ Key.newPub' d

keyTests :: TestTree
keyTests =
  testGroup "Key Tests"
    [ localOption (QC.QuickCheckTests 100) $
        QC.testProperty "ECDSA key recovery" $ \(QC.Positive k) ->
          monadicIO $ do
            let msg = "The quick brown fox ..."

            let (pub, prv) = Key.new' k
            sig <- run $ Key.sign prv msg
            let addr = Address.deriveAddress pub

            let (addr1, addr2) = Address.recoverAddress' sig msg
            assert $ (addr1 == addr) || (addr2 == addr)

    , localOption (QC.QuickCheckTests 10000) $
        QC.testProperty "dehexPub (encodeUtf8 $ hexPub pubKey) == pubKey)" $ \pubKey ->
          let pubKeyHexBS = decodeUtf8 (Key.unHexPub $ Key.hexPub pubKey)
              ePubKey = Key.dehexPub $ encodeUtf8 pubKeyHexBS
          in case ePubKey of
            Left err      -> False
            Right pubKey' -> pubKey == pubKey'

    ]
