{-|

Cryptography.

-}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Key (
  -- ** Key types
  PubKey(..),
  ECDSA.PrivateKey,
  ECDSA.Signature,
  ECDSAKeyPair,

  -- ** Generation
  new,
  new',
  newPub,
  newPub',
  newPriv,

  -- ** Signing
  sign,
  signS,
  signWith,
  signSWith,
  verify,
  InvalidSignature(..),

  -- ** Recovery
  recover,

  -- ** HMAC
  hmacSha256,

  -- ** Curves and Ciphers
  sec_p256k1,
  aes256,
  desEde3,

  validateKey,
  validatePair,
  extractPoint,
  fromPoint,
  toPublic,
  fromSecret,

  -- ** Serialization
  encodeKey,
  decodeKey,

  HexPub,
  HexPriv,

  -- ** Hex Encoding
  hexPriv,
  hexPub,
  unHexPub,
  unHexPriv,
  dehexPriv,
  dehexPub,
  tryDecodePub,
  tryDecodePriv,

  -- ** Fingerprinting
  fingerprint,

  -- ** PEM and x509
  importPriv,
  importPub,

  encodePriv,
  decodePriv,

  exportPriv,
  exportPub,

  pemData,
  pubKeyToAsn,
  encodeDer,
  decodeDer,

  encodeSig,
  putSignature,
  decodeSig,
  decodeSig',
  getSignature,

  getSignatureRS,
  mkSignatureRS,

  -- ** Diffie-Hellman
  DH.SharedKey(..),
  dhGenerateKeyPair,
  dhGetShared,
  dhSecret,
  dhEncode,
  secretToPrivate,

  -- ** Encryption
  encrypt,
  decrypt,

) where

import Protolude hiding (from, to, get, put)
import Safe (toEnumMay)
import Data.Maybe (fromJust)

import Control.Monad.Fail

import qualified Hash
import qualified Encoding
import qualified SafeString as SS
import qualified Utils

import Data.Aeson (ToJSON(..), FromJSON(..), Value(String))
import Data.Aeson.Types (typeMismatch)
import Data.Proxy
import Data.Serialize as S
import Data.ByteArray as B
import qualified Data.PEM as PEM -- pem package
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base64 as BS64
import qualified Data.ByteArray.Encoding as B

import Data.ASN1.Types
import Data.ASN1.Encoding
import Data.ASN1.BitArray
import Data.ASN1.BinaryEncoding
import qualified Data.X509 as X509
import qualified Data.ASN1.Types as ASN1

import Crypto.Hash
import Crypto.Random.Types (MonadRandom(..))
import Crypto.Number.Serialize

import Crypto.Error
import Crypto.Random.Entropy
import Crypto.Data.Padding (Format(..), pad, unpad)

import qualified Crypto.MAC.HMAC as HM
import qualified Crypto.KDF.HKDF as KDF
import qualified Crypto.KDF.PBKDF2 as KDF
import qualified Crypto.PubKey.ECC.DH as DH
import qualified Crypto.PubKey.Ed25519 as ED
import qualified Crypto.PubKey.ECIES as IES

import Crypto.PubKey.ECC.Types
import Crypto.PubKey.ECC.Generate
import Crypto.Number.Generate (generateBetween)
import qualified Crypto.ECC as ECC (encodePoint)
import qualified Crypto.PubKey.ECC.Prim as ECC
import qualified Crypto.PubKey.ECC.Types as ECC
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA

import Crypto.Number.ModArithmetic (inverse)
import Math.NumberTheory.Moduli (sqrtModP)

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.TripleDES (DES_EDE3)
import Crypto.Cipher.Types (Cipher(..), BlockCipher(..), IV, makeIV, blockSize,
  ecbEncrypt, ecbDecrypt)

import Database.PostgreSQL.Simple.FromRow   (FromRow(..), field)
import Database.PostgreSQL.Simple.ToRow     (ToRow(..))
import Database.PostgreSQL.Simple.ToField   (ToField(..), Action(..))
import Database.PostgreSQL.Simple.FromField (FromField(..), ResultError(..), returnError)

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

-- | A serializable ECC public key
-- An ECC pubkey is created by multiplying a generator for the group
-- G with the secret key x:
--
-- > Pub = x G
--
-- The result is serialized as a 33-byte array.
--
-- ECC public keys obey the additively homomorphic property:
--
-- > Pub1 + Pub2 = (x1 + x2 (mod n)) G
newtype PubKey = PubKey ECDSA.PublicKey
  deriving (Show, Eq, Generic, NFData)

instance ToJSON PubKey where
   toJSON = String . decodeUtf8 . unHexPub . hexPub
instance FromJSON PubKey where
  parseJSON (String pk) =
    case dehexPub (encodeUtf8 pk) of
      Left err     -> fail $ "PubKey" <> err
      Right pubKey -> pure pubKey
  parseJSON invalid = typeMismatch "PubKey" invalid

type ECDSAKeyPair = (PubKey, ECDSA.PrivateKey)

instance NFData ECDSA.PublicKey where rnf !_ = ()
instance NFData ECDSA.PrivateKey where rnf !_ = ()
instance NFData ECDSA.Signature where rnf !_ = ()

-- | (pk, sk) <- new
-- Returns a new elliptic curve key pair.
--
-- WARNING: Vulnerable to timing attacks.
new :: IO (PubKey, ECDSA.PrivateKey)
new = do
  (pk, sk) <- generate sec_p256k1
  return (PubKey pk, sk)

-- | Deterministic version of `new`
new' :: Integer -> (PubKey, ECDSA.PrivateKey)
new' d = (PubKey pk, sk)
  where
    curve = sec_p256k1
    q = generateQ curve d
    (pk, sk) = (ECDSA.PublicKey curve q, ECDSA.PrivateKey curve d)

-- | pk <- new
--
-- WARNING: Vulnerable to timing attacks.
newPub :: IO PubKey
newPub = fst <$> new

newPub' :: Integer -> PubKey
newPub' = fst . new'

-- | sk <- new
--
-- WARNING: Vulnerable to timing attacks.
newPriv :: IO ECDSA.PrivateKey
newPriv = snd <$> new

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

-- | Orphan Ord instance for using Signature in a Set
instance Ord ECDSA.Signature where
  s1 `compare` s2 = getSignatureRS s1 `compare` getSignatureRS s2

-- | Sign a message with a private key with ECDSA
--
-- > sig <- sign(sk, m)
--
-- Not deterministic, uses random nonce generation.
-- WARNING: Vulnerable to timing attacks.
sign
  :: (MonadRandom m)
  => ECDSA.PrivateKey
  -> ByteString
  -> m ECDSA.Signature
sign priv msg = do
  k <- generateBetween 1 (n - 1)
  case signWith priv k msg of
    Nothing -> sign priv msg

    Just sig -> return sig
  where
    n = ECC.ecc_n (ECC.common_curve sec_p256k1)

-- Deterministic, uses explicit nonce.
-- WARNING: Vulnerable to timing attacks.
signWith
  :: ECDSA.PrivateKey
  -> Integer
  -> ByteString
  -> Maybe ECDSA.Signature
signWith priv k msg
  | validKeypairKPrecondition priv k msg = signWith_ priv k msg
  | otherwise = Nothing

signWith_
  :: ECDSA.PrivateKey
  -> Integer
  -> ByteString
  -> Maybe ECDSA.Signature
signWith_ priv k msg = ECDSA.signWith k priv SHA3_256 msg

validKeypairKPrecondition :: ECDSA.PrivateKey -> Integer -> ByteString -> Bool
validKeypairKPrecondition priv k msg =
  let pub  = toPublic priv
      sigM = signWith_ priv k msg
  in case sigM of
      Nothing ->
        False

      Just sig ->
        let (c1, c2) = recover_ sig msg
        in if | c1 == pub -> True
              | c2 == pub -> True
              | True      -> False

-- | Verify a message with a public key with ECDSA
--
-- > 0/1 <- verify(pk, m, sig)
verify
  :: PubKey
  -> ECDSA.Signature
  -> ByteString
  -> Bool
verify (PubKey key) sig msg =
  ECDSA.verify SHA3_256 key sig msg

-- | Sign a serializable instance encoding in binary with ECDSA.
--
-- Not deterministic, uses random nonce generation.
-- WARNING: Vulnerable to timing attacks.
signS
  :: Serialize a
  => ECDSA.PrivateKey
  -> a
  -> IO ECDSA.Signature
signS priv msg = sign priv (encode msg)

-- | Sign a serializable instance encoding in binary with ECDSA.
--
-- Deterministic, uses explicit nonce.
-- WARNING: Vulnerable to timing attacks.
signSWith
  :: Serialize a
  => ECDSA.PrivateKey
  -> Integer
  -> a
  -> Maybe ECDSA.Signature
signSWith priv k msg = signWith priv k (encode msg)

-------------------------------------------------------------------------------
-- HMAC
-------------------------------------------------------------------------------

hmacSha256 :: ByteString -> ByteString -> ByteString
hmacSha256 key msg = B.convert (mac key msg)
  where
    mac :: ByteString -> ByteString -> HM.HMAC SHA3_256
    mac = HM.hmac

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

-- | The 256-bit Koblitz elliptic curve over F_p with and verifiably random
-- parameters.
--
-- Reference: http://www.secg.org/SEC2-Ver-1.0.pdf#page=21
--
-- > p   = 0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f
-- > a   = 0x0000000000000000000000000000000000000000000000000000000000000000
-- > b   = 0x0000000000000000000000000000000000000000000000000000000000000007
-- > g_x = 0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798
-- > g_y = 0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8
-- > n   = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141
-- > h   = 0x1
sec_p256k1 :: Curve
sec_p256k1 = ECC.getCurveByName SEC_p256k1

pointSize :: ECC.Curve -> Int
pointSize = toBytes . ECC.curveSizeBits
  where
    toBytes bits = (bits + 7) `div` 8

instance Serialize PubKey where
  put (PubKey key) = put (exportKey key)
  get = get >>= loadKey

encodePriv :: ECDSA.PrivateKey -> ByteString
encodePriv key = encode (ECDSA.private_d key)

decodePriv :: ByteString -> Either [Char] ECDSA.PrivateKey
decodePriv bs = decode bs >>= pure . fromSecret

-- | Export the elliptic curve public key
exportKey :: ECDSA.PublicKey -> (Int, Integer, Integer)
exportKey key = (curve,x,y)
  where
    curve         = fromEnum SEC_p256k1
    ECC.Point x y = ECDSA.public_q key

-- | Validate a public key as a valid secp25k1 ECC key
validateKey :: PubKey -> Bool
validateKey (PubKey key) = ECC.isPointValid sec_p256k1 point
  where
    point = ECDSA.public_q key

-- | Validate that a public-private key pair are derived from each other.
validatePair :: ECDSAKeyPair -> Bool
validatePair (pub, priv) = toPublic priv == pub

-- | Export the elliptic curve point for a public key.
extractPoint :: PubKey -> (Integer, Integer)
extractPoint (PubKey key) = (x,y)
  where
    ECC.Point x y = ECDSA.public_q key

-- | Import the elliptic curve point for a public key. ( Not safe )
fromPoint :: (Integer, Integer) -> PubKey
fromPoint (x,y)
    | ECC.isPointValid sec_p256k1 p = PubKey $ ECDSA.PublicKey sec_p256k1 (ECC.Point x y)
    | otherwise                     = panic "Invalid elliptic curve point"
  where
    p = ECC.Point x y

-- | Create a public key from a secret key
--
-- WARNING: Vulnerable to timing attacks.
toPublic :: ECDSA.PrivateKey -> PubKey
toPublic key = PubKey (ECDSA.PublicKey curve point)
  where
    curve  = ECDSA.private_curve key
    curve' = ECC.common_curve curve
    point  = ECC.pointMul curve (ECDSA.private_d key) g
    g      = ECC.ecc_g curve'

-- | Import the elliptic curve secret for a private key. ( UNSAFE )
fromSecret :: Integer -> ECDSA.PrivateKey
fromSecret d =
  ECDSA.PrivateKey
    { ECDSA.private_curve = sec_p256k1
    , private_d = d
    }

-- | Deserialize a public key encoded with the curve
loadKey
  :: (Int, Integer, Integer)
  -> Get PubKey
loadKey (curve,x,y) = do
  key <- ECDSA.PublicKey <$> curve' <*> pure point
  if ECC.isPointValid sec_p256k1 point
    then return (PubKey key)
    else mzero -- Fail to parse invalid point
  where
    point  = ECC.Point x y
    curve' = pure (ECC.getCurveByName (toEnum curve))

-- | Extract (r,s) parameters of a digital signature ( UNSAFE )
getSignatureRS :: ECDSA.Signature -> (Integer,Integer)
getSignatureRS (ECDSA.Signature r s) = (r,s)

-- | Transform (r,s) into `ECDSA.Signature r s` ( UNSAFE )
mkSignatureRS :: (Integer, Integer) -> ECDSA.Signature
mkSignatureRS (r,s) = ECDSA.Signature r s -- XXX position is invalid; function only used in Script/Eval.hs so far

instance Hash.Hashable PubKey where
  toHash  = Hash.toHash . extractPoint

-------------------------------------------------------------------------------
-- Export
-------------------------------------------------------------------------------

ecOid :: ASN1
ecOid = OID (getObjectID X509.PubKeyALG_EC)

secp256k1Oid :: ASN1
secp256k1Oid = OID [1,3,132,0,10]

ecKey :: X509.SerializedPoint -> X509.PubKeyEC
ecKey = X509.PubKeyEC_Named SEC_p256k1

encodeEc :: X509.SerializedPoint -> ASN1S
encodeEc point = toASN1 (X509.PubKeyEC (ecKey point))

serializePoint :: CurveName -> (Integer, Integer) -> X509.SerializedPoint
serializePoint name (x,y) = pub
  where
    pub   = X509.SerializedPoint bs
    bs    = B.cons 4 (i2ospOf_ bytes x `B.append` i2ospOf_ bytes y)
    bits  = ECC.curveSizeBits (ECC.getCurveByName name)
    bytes = (bits + 7) `div` 8

pubKeyToAsn :: PubKey -> [ASN1]
pubKeyToAsn pub = (encodeEc $ serializePoint SEC_p256k1 (extractPoint pub)) []

uncompressPair :: [ASN1] -> (Integer, (Integer, Integer))
uncompressPair xs =
  case xs of
    [ Start Sequence
      , IntVal 1
      , OctetString priv
      , Start (Container Context 0)
      , secp256k1Oid
      , End (Container Context 0)
      , Start (Container Context 1)
      , BitString (BitArray 520 pub)
      , End  (Container Context 1)
      , End Sequence
      ] ->
        let (x, y) = BS.splitAt 32 (BS.drop 1 pub) in
        (os2ip priv, (os2ip x, os2ip y))
    _ -> panic "Invalid point compression."

compressPair :: (Integer, Integer) -> Integer -> [ASN1]
compressPair (x,y) priv =
  [ Start Sequence
  , IntVal 1
  , OctetString (i2osp priv)
  , Start (Container Context 0)
  , secp256k1Oid
  , End (Container Context 0)
  , Start (Container Context 1)
  , BitString (BitArray 520 pubBits)
  , End  (Container Context 1)
  , End Sequence
  ]
  where
    pubBits = BS.cons 4 (i2osp x <> i2osp y)

uncompressPoint :: [ASN1] -> (Integer, Integer)
uncompressPoint xs =
  case xs of
    [ Start Sequence
      , Start Sequence
      , ecOid
      , secp256k1Oid
      , End Sequence
      , BitString (BitArray 520 pub)
      , End Sequence
      ] ->
        let (x, y) = BS.splitAt 32 (BS.drop 1 pub) in
        (os2ip x, os2ip y)
    _ -> panic "Invalid point compression."

compressPoint :: (Integer, Integer) -> [ASN1]
compressPoint (x,y) =
  [ Start Sequence
    , Start Sequence
    , ecOid
    , secp256k1Oid
    , End Sequence
    , BitString (BitArray 520 pubBits)
    , End Sequence
    ]
  where
    pubBits = BS.cons 4 (i2osp x <> i2osp y)

-- | Import public/private keypair from PEM file
importPriv :: ByteString -> Either Text ECDSAKeyPair
importPriv bs =
  if validateKey point
    then Right (point, priv)
    else Left "Invalid key data. Is not elliptic curve point"
  where
    pem = pemData bs
    Right der = decodeDer pem
    (d, (x0, x1)) = uncompressPair der
    point = fromPoint (x0, x1)
    priv = fromSecret d

-- | Import public key from PEM file
importPub :: ByteString -> Either Text PubKey
importPub bs =
  case decodeDer (pemData bs) of
    Left err -> Left "Invalid PEM format"
    Right der -> do
      let (x0, x1) = uncompressPoint der
      let point = fromPoint (x0, x1)
      if validateKey point
        then Right point
        else Left "Invalid key data. Is not elliptic curve point"

-- | Export public/private keypair to PEM file
exportPriv :: ECDSA.PrivateKey -> ByteString
exportPriv priv = do
  let (x0, x1) = extractPoint $ toPublic priv
  let asn1 = encodeDer $ compressPair (x0, x1) (ECDSA.private_d priv)
  pemPrivate asn1

-- | Export public key to PEM file
exportPub :: PubKey -> ByteString
exportPub pub = do
  let (x0, x1) = extractPoint pub
  let asn1 = encodeDer $ compressPoint (x0, x1)
  pemPublic asn1

-------------------------------------------------------------------------------
-- Binary Encoding
-------------------------------------------------------------------------------

-- | Binary serialize public key ( UNSAFE )
encodeKey :: PubKey -> ByteString
encodeKey = encode

-- | Binary deserialize public key ( UNSAFE )
decodeKey :: ByteString -> Either [Char] PubKey
decodeKey = decode

{- Serialization Note:

   Binary encoding of signatures tightly reflects the binary encoding of
   signatures that are serialized and sent from the SDKs. Modifying this code
   and/or putSafeString & getSafeString may cause deserialization of signatures
   sent from the SDKs to fail.

-}

putSignature :: S.Putter ECDSA.Signature
putSignature (ECDSA.Signature r s) = do
    S.put $ safeEncInteger r
    S.put ':'
    S.put $ safeEncInteger s
  where
    safeEncInteger :: Integer -> SS.SafeString
    safeEncInteger = SS.fromBytes' . show

getSignature :: S.Get ECDSA.Signature
getSignature = do
  rSS <- S.get
  get :: Get Char
  sSS <- S.get
  let rBS = SS.toBytes rSS
  let sBS = SS.toBytes sSS
  let read' = head . reads . BSC.unpack
  let mRS = do
        r' <- read' rBS
        s' <- read' sBS
        return (fst r', fst s')
  case mRS of
    Nothing -> fail "Could not decode ECDSA.signature"
    Just (r,s) -> return $ ECDSA.Signature r s

data InvalidSignature
  = InvalidSignature ECDSA.Signature ByteString
  | DecodeSignatureFail ByteString
  | SignatureSplittingFail ByteString
  deriving (Show, Eq, Generic, S.Serialize, NFData)

-- XXX Wrap ECDSA.Signature in newtype to prevent orphan instances
instance Serialize ECDSA.Signature where
  put = put . encodeSig
  get = do
    sigBS <- get
    case sigBS of
      Left err -> fail err
      Right sig -> pure sig

-- | Binary encoding of a signature
encodeSig :: ECDSA.Signature -> ByteString
encodeSig = Encoding.base64P . runPut . putSignature

-- Binary decoding of a signature
decodeSig :: ByteString -> Either InvalidSignature ECDSA.Signature
decodeSig bs = first (DecodeSignatureFail . toS) $
  runGet getSignature =<< Encoding.unbase64P bs

decodeSig' :: ByteString -> ECDSA.Signature
decodeSig' bs =
  case decodeSig bs of
    Left err -> panic $ show err
    Right sig -> sig

-------------------------------------------------------------------------------
-- Hex Encoding
-------------------------------------------------------------------------------

-- | Hexadecimal encoded public key
newtype HexPub = HexPub ByteString
  deriving (Eq, Ord, Show, Generic, Serialize, Hash.Hashable)

-- | Hexadecimal encoded private key
newtype HexPriv = HexPriv ByteString
  deriving (Eq, Ord, Show, Generic, Serialize, Hash.Hashable)

-- | Convert HexPub to raw Bytesstring
unHexPub :: HexPub -> ByteString
unHexPub (HexPub x) = x

-- | Convert HexPriv to raw BytesString
unHexPriv :: HexPriv -> ByteString
unHexPriv (HexPriv x) = x

-- | Hex encoding of prviate key
hexPriv :: ECDSA.PrivateKey -> HexPriv
hexPriv = HexPriv . Encoding.base16 . i2osp . ECDSA.private_d

-- | Hex encoding of public key
hexPub :: PubKey -> HexPub
hexPub pub = HexPub (Encoding.base16 ((i2ospOf_ 32 x) <> (i2ospOf_ 32 y)))
  where
    (x, y) = extractPoint pub

-- | Dehex private key
dehexPriv :: ByteString -> Either [Char] ECDSA.PrivateKey
dehexPriv bs = ECDSA.PrivateKey <$> pure sec_p256k1 <*> private_num
  where
     private_num = os2ip <$> ((B.convertFromBase B.Base16 bs') :: Either [Char] ByteString)
     bs' = if odd (BS.length bs)
            then "0" <> bs
            else bs
    -- B.convertFromBase B.Base16 only likes even lengthed bytestrings...

-- | Dehex public key
dehexPub :: ByteString -> Either [Char] PubKey
dehexPub bs = do
  bs' <- B.convertFromBase B.Base16 bs
  let (xs, ys) = BS.splitAt 32 bs'
  let point = ECC.Point (os2ip xs) (os2ip ys)
  case ECC.isPointValid sec_p256k1 point of
    False -> Left "dehexPub: Invalid public key point"
    True  -> Right (PubKey (ECDSA.PublicKey sec_p256k1 point))

tryDecodePub :: ByteString -> Either Text PubKey
tryDecodePub pubBS = case dehexPub pubBS of
  Left _ -> case importPub pubBS of
    Right pub -> Right pub
    Left _ -> Left "Failed to decode ECDSA public key from hex or pem format."
  Right pub -> Right pub

tryDecodePriv :: ByteString -> Either Text ECDSA.PrivateKey
tryDecodePriv privBS = case dehexPriv privBS of
   Left _ -> case importPriv privBS of
     Right (_,priv) -> Right priv
     Left _ -> Left "Failed to decode ECDSA private key from hex or pem format."
   Right priv -> Right priv


-------------------------------------------------------------------------------
-- DER / PEM Encoding
-------------------------------------------------------------------------------

-- | Deocde DER data
decodeDer :: Maybe BS.ByteString -> Either [Char] [ASN1]
decodeDer (Just bs) =
  case decodeASN1' DER bs of
    Left _ -> Left "couldn't decode ASN1 stream"
    Right r -> Right r
decodeDer Nothing = Left "decodeDer: bytestring was Nothing"

-- | Encode DER data
encodeDer :: [ASN1] -> ByteString
encodeDer = encodeASN1' DER

encryptedHeader :: ByteString -> [([Char], ByteString)]
encryptedHeader iv = [
    {-("Proc-Type", "4,ENCRYPTED")-}
  {-, ("DEK-Info", "DES-EDE3-CBC" <> "," <> iv)-}
  ]

-- | Write a private elliptic curve key PEM file
pemPrivate :: ByteString -> ByteString
pemPrivate asn1 = PEM.pemWriteBS pem
  where
    pem =
      PEM.PEM
      { PEM.pemName = "EC PRIVATE KEY"
      {-, PEM.pemHeader = encryptedHeader ""-}
      , PEM.pemHeader = []
      , PEM.pemContent = asn1
      }

-- | Write a public elliptic curve key PEM file
pemPublic :: ByteString -> ByteString
pemPublic asn1 = PEM.pemWriteBS pem
  where
    pem =
      PEM.PEM
      { PEM.pemName = "EC PUBLIC KEY"
      , PEM.pemHeader = []
      , PEM.pemContent = asn1
      }

pemSignature :: [ASN1] -> ByteString
pemSignature asn1 = PEM.pemWriteBS pem
  where
    pem =
      PEM.PEM
      { PEM.pemName = "EC SIGNATURE"
      , PEM.pemHeader = []
      , PEM.pemContent = encodeDer asn1
      }

-- | Extract the body of a PEM datastream by parsing
pemData :: ByteString -> Maybe ByteString
pemData bs =
  case PEM.pemParseBS bs of
    Right [pem] -> Just $ PEM.pemContent pem
    Right _     -> Nothing
    Left  _     -> Nothing

-------------------------------------------------------------------------------
-- Fingerprinting
-------------------------------------------------------------------------------

-- | Produce the SHA256 fingerprint of a public key.
fingerprint :: PubKey -> [Char]
fingerprint pub = intercalate ":" (fmap Utils.showHex bits)
  where
    bits  = Utils.toByteList (Hash.sha256Raw str)
    str   = i2osp x <> i2osp y
    (x,y) = extractPoint pub

-------------------------------------------------------------------------------
-- ECDSA PubKey Recovery
-------------------------------------------------------------------------------

-- ECDSA Key Recovery
--
-- First, you find the two points R, R′ which have the value r as the
-- x-coordinate r.
--
-- You also compute r^{−1}, which is the multiplicative inverse of the value r
-- from the signature (modulo the order of the generator of the curve).
--
-- Then, you compute z which is the lowest n bits of the hash of the message
-- (where n is the bit size of the curve).
--
-- The two public key canidates are then:
--
-- > Q = r^{-1}(sR - zG)
-- > Q = r^{-1}(sR' - zG)
--
-- See: https://github.com/bitcoin-core/secp256k1/blob/master/src/modules/recovery/main_impl.h
-- Reference: http://www.secg.org/sec1-v2.pdf#p=47

-- | Recover a public key from a signature and message hash.
--
-- WARNING: Vulnerable to timing attacks.
recover
  :: ECDSA.Signature
  -> ByteString
  -> (PubKey, PubKey)
recover sig msg = recover_ sig msg

-- Only used in order to determine which recovered key is the correct one
-- in order to preprend the signature with a 0 (first key) or 1 (second key)
recover_
  :: ECDSA.Signature
  -> ByteString
  -> (PubKey, PubKey)
recover_ sig@(ECDSA.Signature r s) msg =
  let hash = os2ip (Hash.sha256Raw msg)
      [x0] = fmap (\x -> mod x p) $ takeWhile (<p) [(r + i*n) | i <- [0..h]]
      Just invr = inverse r n
      Just y0 = sqrtModP (x0^3 + a*x0 + b) p
      p0 = ECC.Point x0 y0
      q0 = ECC.pointAddTwoMuls sec_p256k1 (invr * s) p0
                                          (invr * (-hash)) g

      c1 = PubKey (ECDSA.PublicKey sec_p256k1 q0)

      p0' = ECC.pointMul sec_p256k1 (-1) p0
      q0' = ECC.pointAddTwoMuls sec_p256k1 (invr * s) p0'
                                           (invr * (-hash)) g
      c2 = (PubKey (ECDSA.PublicKey sec_p256k1 q0'))

  in (c1, c2)

  where
    curve = (ECC.common_curve sec_p256k1)
    ECC.CurveFP pcurve = sec_p256k1
    n = ECC.ecc_n curve
    a = ECC.ecc_a curve
    b = ECC.ecc_b curve
    p = ECC.ecc_p pcurve
    h = ECC.ecc_h curve
    g = ECC.ecc_g curve

-------------------------------------------------------------------------------
-- Diffie-Hellman
-------------------------------------------------------------------------------

-- XXX: redundent type definitions

data ECDHParams = ECDHParams ECC.Curve ECC.CurveName
  deriving (Show,Eq)

dhGenerateKeyPair :: IO (ECDSA.PrivateKey, PubKey)
dhGenerateKeyPair = do
  secret <- DH.generatePrivate sec_p256k1
  let point = DH.calculatePublic sec_p256k1 secret
      pub   = PubKey (ECDSA.PublicKey sec_p256k1 point)
      priv  = ECDSA.PrivateKey sec_p256k1 secret
  return (priv, pub)

dhGetShared :: ECDSA.PrivateKey -> PubKey -> Maybe DH.SharedKey
dhGetShared priv (PubKey pub)
  | ECC.isPointValid curve point = Just $ DH.getShared curve secret point
  | otherwise                    = Nothing
  where
    curve  = sec_p256k1
    point  = ECDSA.public_q pub
    secret = ECDSA.private_d priv

secretToPrivate :: DH.SharedKey -> ECDSA.PrivateKey
secretToPrivate s1 =
  ECDSA.PrivateKey
    { ECDSA.private_curve = sec_p256k1
    , private_d = os2ip s1 }

dhSecret :: ECDSA.PrivateKey -> DH.SharedKey
dhSecret priv = DH.getShared sec_p256k1 (ECDSA.private_d priv) point
  where
    point = DH.calculatePublic sec_p256k1 (ECDSA.private_d priv)

dhEncode :: ECDSA.PrivateKey -> ByteString
dhEncode priv = Encoding.base16 $ B.convert $ dhSecret priv

-------------------------------------------------------------------------------
-- Block Ciphers
-------------------------------------------------------------------------------

blockLength :: Int
blockLength = blockSize aes256

-- | AES Cipher proxy
aes256 :: AES256
aes256 = undefined

-- | 3DES Cipher proxy
desEde3 :: DES_EDE3
desEde3 = undefined

cipherInitNoErr :: BlockCipher c => ScrubbedBytes -> c
cipherInitNoErr k = case cipherInit k of
  CryptoPassed a -> a
  -- If we can't initialize cipher, something is wrong upstream so abort.
  CryptoFailed e -> panic (show e)

prepareKey :: ScrubbedBytes -> ScrubbedBytes
prepareKey key =
  if B.length key == 16 || B.length key == 32
    then key
    else pad (PKCS7 blockLength) key

-- | Draw entropy from random monad for cipher initialization
makeEntropy :: MonadRandom m => Int -> m ByteString
makeEntropy = getRandomBytes

-- | Encrypt using AES256-CBC using computed elliptic curve shared secret of
-- the underlying private key, computed using ECDH.
-- The key must be less than 32 bytes in length
--
-- > encrypt(key, msg)
encrypt :: MonadRandom m => ScrubbedBytes -> ByteString -> m ByteString
encrypt key msg = do
  bs <- makeEntropy blockLength
  let Just iv = makeIV bs :: Maybe (IV AES256)
  let key' = prepareKey key
  let msg' = pad (PKCS7 blockLength) msg
  let ctx = cipherInitNoErr key'
  return $ bs <> (cbcEncrypt ctx iv msg')

-- | Decrypt using AES256-CBC
--
-- > decrypt(key, msg)
decrypt :: MonadRandom m => ScrubbedBytes -> ByteString -> m (Maybe ByteString)
decrypt key msg = do
  bs <- makeEntropy blockLength
  let (iv, msg') = B.splitAt blockLength msg
  let Just iv' = makeIV iv :: Maybe (IV AES256)
  let key' = prepareKey key
  let ctx = cipherInitNoErr key'
  let msg'' = cbcDecrypt ctx iv' msg'
  return (unpad (PKCS7 blockLength) msg'')

-------------------------------------------------------------------------------
-- Postgres DB
-------------------------------------------------------------------------------

instance FromField PubKey where
  fromField f mdata = do
    bs <- fromField f mdata
    case S.decode <$> bs of
      Nothing             -> returnError UnexpectedNull f ""
      Just (Left err)     -> returnError ConversionFailed f (toS err)
      Just (Right pubkey) -> return pubkey

instance ToField PubKey where
  toField = EscapeByteA . S.encode

instance ToRow PubKey where
  toRow pubKey = [toField pubKey]

instance FromRow PubKey where
  fromRow = field
