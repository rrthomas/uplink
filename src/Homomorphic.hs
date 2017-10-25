{-|

Partial homomorphic encryption is a form of encryption that allows limited set
of computations to be carried out on ciphertext, thus generating an encrypted
result which, when decrypted, matches the result of operations performed on the
plaintext.

Paillier encryption to perform homomorphic arithmetic on encrypted data. It is a
partially homomorphic encryption system.

> Decode(Encode(a) + Encode(b)) = a + b
> Decode(Encode(a) * b) = a * b

Reference: https://github.com/google/encrypted-bigquery-client/blob/master/src/paillier.py

-}

{-# OPTIONS_GHC -fno-warn-identities #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Homomorphic (
  -- ** Types
  PlainText,
  CipherText(..),
  PubKey,
  PrvKey,
  rsaKeySize,

  -- ** Encryption
  encrypt,
  _encrypt,
  decrypt,

  -- ** Operations
  cipherAdd,
  cipherSub,
  cipherMul,
  posNeg,

  -- ** Key Gen
  genRSAKeyPair,
  genRSAKeyPairSafe,

  -- ** Testing
  testHomo,
) where

import Protolude

import qualified SafeInteger

import Crypto.Number.Prime (generatePrime)
import Crypto.Number.Generate (generateBetween)
import Crypto.Number.ModArithmetic (inverse, expSafe)

-------------------------------------------------------------------------------
-- Encryption
-------------------------------------------------------------------------------

-- | Plaintext numerical quantity
type PlainText = Integer

-- | Encrypted numerical quantity
newtype CipherText = CipherText Integer
  deriving (Show, Num, Enum, Integral, Real, Ord, Eq)

-- | RSA public key
data PubKey = PubKey
  { bits      :: Int       -- ^ e.g., 2048
  , nModulo   :: Integer   -- ^ n = pq
  , generator :: Integer   -- ^ generator = n+1
  , nSquare   :: Integer   -- ^ n^2
  } deriving (Show)

deriving instance Generic PubKey
instance NFData PubKey

-- | RSA private key
data PrvKey = PrvKey
  { lambda :: Integer    -- ^ lambda(n) = lcm(p-1, q-1)
  , x      :: Integer
  } deriving (Show)

data RSAKeyGenError
  = TooManyBits
  | RSAKeyGenFail
  deriving (Eq, Show)

instance Exception RSAKeyGenError

rsaKeySize :: Int
rsaKeySize = 2048

-- | For our purposes, public keys should not have more bits than SafeInteger.maxBits,
-- otherwise intermediate evaluation of homomorphic operations in Eval.hs would lose
-- information by casting n > SafeInteger.maxBit integers to SafeIntegers which can
-- only represent integers with SafeInteger.maxBits number of bits or less
--
-- The reason for `nBits*2 > SafeInteger.maxBits` is because the finite field (Zq) in
-- which the RSA key pair algorithm and homomorphic operations are computed inside of
-- is of order `q = 2*nBits`.
genRSAKeyPair :: Int -> IO (PubKey, PrvKey)
genRSAKeyPair nBits = do
  -- Generate random primes.
  p <- generatePrime (nBits `div` 2)
  q <- generatePrime (nBits `div` 2)
  let modulo = p*q
  let g = modulo+1
  -- Public key parameters
  let square = modulo*modulo
  -- Private key parameters
  -- let phi_n = (p-1)*(q-1)
  let phi_n = lcm (p-1) (q-1)
  let mu = inverse ((expSafe g phi_n square - 1) `div` modulo) modulo
  case mu of
    Nothing -> panic $ show RSAKeyGenFail -- Impossible
    Just x -> do
      let pub = PubKey {bits=nBits, nModulo=modulo, generator=g, nSquare=square}
      let prv = PrvKey {lambda=phi_n, x=x}
      return (pub, prv)

genRSAKeyPairSafe :: Int -> IO (Either RSAKeyGenError (PubKey, PrvKey))
genRSAKeyPairSafe nBits
  | nBits*2 > SafeInteger.maxBits = pure $ Left $ TooManyBits
  | otherwise = Right <$> genRSAKeyPair nBits

-- | Deterministic version of encryption
_encrypt :: PubKey -> PlainText -> Integer -> CipherText
_encrypt pubKey plaintext r = CipherText result
  where
    result = (g_m*r_n) `mod` n_2
    n_2 = nSquare pubKey
    g_m = expSafe (generator pubKey) plaintext n_2
    r_n = expSafe r (nModulo pubKey) n_2

obfuscate :: PubKey -> Integer -> IO Integer
obfuscate pubKey guess =
  if guess >= nModulo pubKey || (gcd (nModulo pubKey) guess > 1)
  then do
    nextGuess <- generateBetween 1 (nModulo pubKey -1)
    obfuscate pubKey nextGuess
  else
    return guess

-------------------------------------------------------------------------------
-- Encryption
-------------------------------------------------------------------------------

-- | Create homomorphically encrypted cryptotext of an integer.
encrypt :: PubKey -> PlainText -> IO CipherText
encrypt pubKey plaintext = do
  r <- obfuscate pubKey (nModulo pubKey)
  return $ _encrypt pubKey plaintext r

-- | Homomorphically decrypt an integer.
decrypt :: PrvKey -> PubKey -> CipherText -> PlainText
decrypt prvKey pubKey (CipherText ciphertext) =
  let c_lambda = expSafe ciphertext (lambda prvKey) (nSquare pubKey)
      l_c_lamdba = (c_lambda - 1) `div` nModulo pubKey
  in  l_c_lamdba * x prvKey `mod` nModulo pubKey

-------------------------------------------------------------------------------
-- Homomorphic Operations
-------------------------------------------------------------------------------

-- | Encoded addition
-- E(a + b) given E(a) and E(b).
cipherAdd :: PubKey -> CipherText -> CipherText -> CipherText
cipherAdd pubKey (CipherText c1) (CipherText c2) = CipherText res
  where
    res = c1*c2 `mod` nSquare pubKey

-- | Encoded subtraction
-- E(a - b) given E(a) and E(b).
cipherSub :: PubKey -> CipherText -> CipherText -> CipherText
cipherSub pubKey c1 c2 = cipherAdd pubKey c1 (cipherMul pubKey c2 (-1))

-- | Scalar multiplication
-- E(a * b) given E(a) and b
cipherMul :: PubKey -> CipherText -> PlainText -> CipherText
cipherMul pubKey (CipherText c1) p1 = CipherText (expSafe c1 p1 (nSquare pubKey))

-- | Possibly negative number
posNeg :: PubKey -> Integer -> Integer -> Integer
posNeg pub threshold n
  | abs n > threshold = negate (nModulo pub - n)
  | otherwise         = n

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

testHomo :: IO ()
testHomo = do
  (pub, prv) <- genRSAKeyPair 256
  a <- encrypt pub 10000
  b <- encrypt pub 5500
  let res = cipherSub pub a b
  print $ posNeg pub 100000 $ decrypt prv pub res
