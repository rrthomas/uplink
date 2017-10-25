module TestPaillier (
  paillierTests,
) where

import Test.Tasty
import Test.QuickCheck.Monadic
import qualified Test.Tasty.QuickCheck as QC

import Data.Int (Int64)
import qualified Key
import qualified Block
import qualified Account
import qualified Address
import qualified Homomorphic as Hom

paillierTests :: TestTree
paillierTests =
  testGroup "Paillier Tests"
    [
      QC.testProperty "a + b = b + a" $
        (\a b -> (a :: Int64) >= 0 && (b :: Int64) >= 0 QC.==> monadicIO $ do
          (pub, prv) <- run $ Hom.genRSAKeyPair 256
          let a' = toInteger a
              b' = toInteger b
          a'' <- run $ Hom.encrypt pub a'
          b'' <- run $ Hom.encrypt pub b'
          let res1 = Hom.cipherAdd pub a'' b''
              res2 = Hom.cipherAdd pub b'' a''
              res1' = Hom.decrypt prv pub res1
              res2' = Hom.decrypt prv pub res2

          assert $ res1 == res2 && fromInteger res2' == a + b),

      QC.testProperty "a + (b + c) = (a + b) + c" $
        (\a b c -> (a :: Int64) >= 0 && (b :: Int64) >= 0 && (c :: Int64) >= 0 QC.==> monadicIO $ do
          (pub, prv) <- run $ Hom.genRSAKeyPair 256
          let a' = toInteger a
              b' = toInteger b
              c' = toInteger c
          a'' <- run $ Hom.encrypt pub a'
          b'' <- run $ Hom.encrypt pub b'
          c'' <- run $ Hom.encrypt pub c'
          let res1 = Hom.cipherAdd pub a'' $ Hom.cipherAdd pub b'' c''
              res2 = Hom.cipherAdd pub (Hom.cipherAdd pub a'' b'') c''
              res1' = Hom.decrypt prv pub res1
              res2' = Hom.decrypt prv pub res2

          assert $ res1' == res2' && fromInteger res2' == a + b + c),

      QC.testProperty "a * b = b * a" $
        (\a b -> (a :: Int64) >= 0 && (b :: Int64) >= 0 QC.==> monadicIO $ do
          (pub, prv) <- run $ Hom.genRSAKeyPair 256
          let a' = toInteger a
              b' = toInteger b
          a'' <- run $ Hom.encrypt pub a'
          b'' <- run $ Hom.encrypt pub b'
          let res1 = Hom.cipherMul pub a'' b'
              res2 = Hom.cipherMul pub b'' a'
              res1' = Hom.decrypt prv pub res1
              res2' = Hom.decrypt prv pub res2

          assert $ res1' == res2' && fromInteger res2' == a * b),

      QC.testProperty "a * (b * c) = (a * b) * c" $
        (\a b c -> (a :: Int64) >= 0 && (b :: Int64) >= 0 && (c :: Int64) >= 0 QC.==> monadicIO $ do
          (pub, prv) <- run $ Hom.genRSAKeyPair 256
          let a' = toInteger a
              b' = toInteger b
              c' = toInteger c
          a'' <- run $ Hom.encrypt pub a'
          b'' <- run $ Hom.encrypt pub b'
          c'' <- run $ Hom.encrypt pub c'
          let res1 = Hom.cipherMul pub (Hom.cipherMul pub b'' c') a' -- valid due to above
              res2 = Hom.cipherMul pub (Hom.cipherMul pub a'' b') c'
              res1' = Hom.decrypt prv pub res1
              res2' = Hom.decrypt prv pub res2

          assert $ res1' == res2' && fromInteger res2' == a * b * c),

      QC.testProperty "a - b" $
        (\a b -> (a :: Int64) >= 0 && (b :: Int64) >= 0 QC.==> monadicIO $ do
          (pub, prv) <- run $ Hom.genRSAKeyPair 256
          let a' = toInteger a
              b' = toInteger b
          a'' <- run $ Hom.encrypt pub a'
          b'' <- run $ Hom.encrypt pub b'
          let res1 = Hom.cipherSub pub a'' b''
              threshold = toInteger (maxBound :: Int64)
              res1' = Hom.posNeg pub threshold $
                        Hom.decrypt prv pub res1

          assert $ fromInteger res1' == a - b)
  ]
