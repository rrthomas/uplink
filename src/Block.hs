{-|

Block data structures and operations.

-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Block (
  Block(..),
  BlockHeader(..),
  BlockSignature(..),
  BlockSignatures,
  InvalidBlock(..),
  InvalidBlockReason(..),

  sortBlocks,
  medianTimestamp,

  -- ** Creation
  newBlock,
  genesisBlock,

  -- ** Hashing
  hashBlock,

  -- ** Validation & Verification
  validateBlock,
  validateBlockDB,
  validateChain,
  verifyBlockSig,

  -- ** Query nested fields
  getConsensus,
  getValidatorSet,
  getTimestamp,

  -- ** Serialization
  encodeBlock,
  decodeBlock,

) where

import Protolude

import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:))
import Data.Aeson.Types (Value(..), typeMismatch)
import Data.Serialize (Serialize)
import qualified Data.Serialize as Serialize
import qualified Data.Binary as B
import qualified Data.Set as Set

import qualified Crypto.Hash.MerkleTree as Merkle

import Hash (Hash)
import Address (Address, AAccount)
import Transaction (Transaction)
import qualified Key
import qualified Hash
import qualified Time
import qualified Address
import qualified Encoding
import qualified Transaction as Tx
import qualified Consensus.Authority.Params as CAP

import Database.PostgreSQL.Simple.ToField   (ToField(..), Action(..))
import Database.PostgreSQL.Simple.FromField (FromField(..), ResultError(..), returnError)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Block = Block
  { index        :: Int             -- ^ Block index
  , header       :: BlockHeader     -- ^ Block header
  , signatures   :: BlockSignatures -- ^ Set of Block signatures
  , transactions :: [Transaction]   -- ^ Block transactions,
  } deriving (Show, Eq, Generic, NFData, B.Binary, Serialize)

data BlockHeader = BlockHeader
  { origin     :: Address AAccount                -- ^ Validator of the block
  , prevHash   :: Hash Encoding.Base16ByteString  -- ^ The hash value of the previous block
  , merkleRoot :: ByteString                      -- ^ Merkle tree collection which is a hash of all transactions related to this block
  , timestamp  :: Time.Timestamp                  -- ^ A Unix timestamp recording when this block was created
  , consensus  :: CAP.PoA                         -- ^ Consensus algorithm to verify next block
  } deriving (Show, Eq, Generic, NFData, B.Binary, Serialize)

instance Ord Block where
  compare = compare `on` index

data BlockSignature = BlockSignature
  { signature  :: Key.Signature
  , signerAddr :: Address AAccount
  } deriving (Show, Eq, Ord, Generic, B.Binary, NFData)

type BlockSignatures = Set BlockSignature

medianTimestamp :: [Block] -> Either Text Time.Timestamp
medianTimestamp []  = Left "medianTimestamp: empty list of blocks"
medianTimestamp [b] = Right $ timestamp $ header b
medianTimestamp blks
  | even lenBlks =
      let idx = lenBlks `div` 2 in
      case atMay blkTimestamps idx of
        Nothing -> Left $
          "medianTimestamp: failed to find block at index " <> show idx
        Just ts -> Right ts
  | otherwise =
       let idx = lenBlks `div` 2
           ts1 = atMay blkTimestamps idx
           ts2 = atMay blkTimestamps (idx + 1) in
       case (,) <$> ts1 <*> ts2 of
         Nothing -> Left $
          "medianTimestamp: failed to find block at index " <> show idx
         Just (ts1,ts2) -> Right $
           fromIntegral $ round $ fromIntegral (ts1 + ts2) / 2
  where
    lenBlks = length blks
    blkTimestamps = sort $
      map (timestamp . header) blks

instance Hash.Hashable Block where
instance Hash.Hashable BlockHeader where
  -- TODO: Shouldn't we use this type sig instead?
  -- toHash = Hash.toHash . Encoding.encodeBase16 . (toS :: [Char] -> ByteString) . show
  toHash = Hash.toHash . (toS :: [Char] -> ByteString) . show

instance Hash.Hashable BlockSignature where
  toHash = Hash.toHash . Encoding.encodeBase64P . (toS :: [Char] -> ByteString) . show

hashBlockHeader :: BlockHeader -> Hash Encoding.Base16ByteString
hashBlockHeader = Hash.toHash

-- | Hash block header
hashBlock :: Block -> Hash Encoding.Base16ByteString
hashBlock = hashBlockHeader . header

-------------------------------------------------------------------------------
-- Block Construction
-------------------------------------------------------------------------------

newBlock
  :: Address AAccount                 -- ^ origin
  -> Hash Encoding.Base16ByteString   -- ^ prevBlock hash
  -> [Transaction]                    -- ^ transaction list
  -> Int                              -- ^ block index
  -> Key.PrivateKey                   -- ^ signature
  -> CAP.PoA                          -- ^ Consensus alg
  -> IO Block
newBlock origin prevBlockHash txs n priv poa = do
  ts <- Time.now
  let htxs = fmap (Hash.getRawHash . Tx.hashTransaction) txs
      header = BlockHeader {
        origin     = origin
      , prevHash   = prevBlockHash
      , merkleRoot = Merkle.mtHash (Merkle.mkMerkleTree htxs)
      , timestamp  = ts
      , consensus  = poa
      }
  let headerHash = hashBlockHeader header
  let accAddr = Address.deriveAddress $ Key.toPublic priv
  sig <- Key.sign priv (Hash.getRawHash headerHash)
  let blockSig = BlockSignature sig accAddr
  return Block {
    header       = header
  , index        = n
  , signatures   = Set.singleton blockSig
  , transactions = txs
  }

genesisBlock
  :: ByteString     -- ^ Genesis block seed
  -> Time.Timestamp -- ^ timestamp of initial block
  -> CAP.PoA        -- ^ base consensus algorithm
  -> IO Block
genesisBlock seed ts genPoa =
    return Block
      { header       = genesisHeader
      , signatures   = Set.empty
      , index        = 0
      , transactions = []
      }
  where
    genesisHeader = BlockHeader
      { origin     = Address.emptyAddr
      , prevHash   = Hash.toHash seed
      , merkleRoot = Merkle.getMerkleRoot Merkle.emptyHash
      , timestamp  = ts
      , consensus  = genPoa
      }

-- | Sort blocks based on index
sortBlocks :: [Block] -> [Block]
sortBlocks = sortBy (compare `on` index)

-------------------------------------------------------------------------------
-- Block Validation (Chain Rules)
-------------------------------------------------------------------------------

data InvalidBlock = InvalidBlock Int InvalidBlockReason
  deriving (Show, Eq)

data InvalidBlockReason
  = InvalidBlockSignature Key.InvalidSignature
  | InvalidBlockSigner (Address AAccount)
  | InvalidBlockOrigin (Address AAccount)
  | InvalidPrevBlockHash (Hash Encoding.Base16ByteString) (Hash Encoding.Base16ByteString)
  | InvalidBlockTimestamp Time.Timestamp
  | InvalidMedianTimestamp Text -- This shouldn't happen
  | InvalidBlockMerkleRoot Int ByteString ByteString
  | InvalidBlockTx Tx.InvalidTransaction
  deriving (Show, Eq)

-- | Ensure the integrity of a block, without respect to World state
--
-- 1. All transactions are ordered ???
-- 2. Hashed properly
-- 3. Have sensible timestamps
--    - is the block timestamp > median timestamp of past 11 blocks
-- 4. Transaction list matches hash
-- 5. Merkle root validated
-- 6. Previous block hash is correct
validateBlock :: Time.Timestamp -> Block -> Block -> Either InvalidBlock ()
validateBlock medianTs prevBlock Block{..} =
    first (InvalidBlock index) $ do
      validateMerkleRoot
      validateTimestamp
      validatePrevHash
      validateTransactions
  where
    txHashes = map (Hash.getRawHash . Tx.hashTransaction) transactions
    mRoot    = Merkle.mtHash $ Merkle.mkMerkleTree txHashes
    blockTs  = timestamp header

    validateMerkleRoot
      | mRoot == merkleRoot header = Right ()
      | otherwise = Left $ InvalidBlockMerkleRoot
          index mRoot (merkleRoot header)

    validateTimestamp
      | blockTs > medianTs = Right ()
      | otherwise = Left $ InvalidBlockTimestamp blockTs

    prevHash' = hashBlock prevBlock
    validatePrevHash
      | prevHash header == prevHash' = Right ()
      | otherwise = Left $ InvalidPrevBlockHash (prevHash header) prevHash'

    validateTransactions =
      first InvalidBlockTx $
        mapM_ Tx.validateTransaction transactions

-- | Only used when validating a block being read from DB
-- XXX: Come up with block integrity check, previous one not correct
validateBlockDB :: Block -> Bool
validateBlockDB block =
    either (const False) (const True) . sequence $
      map Tx.validateTransaction (transactions block)
  where
    blockTs = timestamp $ header block

validateChain :: [Block] -> IO (Either InvalidBlock ())
validateChain [] = pure $ Right ()
validateChain blks@(b:bs) = do
  case medianTimestamps of
    Left err -> pure $ Left $
      InvalidBlock (index b) $ InvalidMedianTimestamp $ toS err
    Right tss ->
      let validBlocks = zipWith validateBlock' tss descBlkPairs
       in case sequence validBlocks of
            Left err -> return $ Left err
            Right _  -> return $ Right ()
  where
    descBlks = sortBy (flip compare) blks
    descBlkPairs = zip (fromMaybe [] $ tailMay descBlks) descBlks
    validateBlock' ts (pb,b) = validateBlock ts pb b

    elevens :: [Block] -> [[Block]]
    elevens [] = []
    elevens ys@(x:xs) = take 11 ys : elevens xs

    medianTimestamps = mapM medianTimestamp $ elevens descBlks

verifyBlockSig
  :: Key.PubKey
  -> Key.Signature
  -> Block
  -> Either Key.InvalidSignature ()
verifyBlockSig pubKey sig block
  | Key.verify pubKey sig encodedBlockHash = pure ()
  | otherwise = Left $ Key.InvalidSignature sig encodedBlockHash
  where
    encodedBlockHash = Hash.getRawHash $ hashBlock block

-------------------------------------------------------------------------------
-- Querying nested fields
-------------------------------------------------------------------------------

getConsensus :: Block -> CAP.PoA
getConsensus = consensus . header

getValidatorSet :: Block -> CAP.ValidatorSet
getValidatorSet = CAP.validatorSet . getConsensus

getTimestamp :: Block -> Time.Timestamp
getTimestamp = timestamp . header

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

encodeBlock :: Block -> ByteString
encodeBlock = Serialize.encode

decodeBlock :: ByteString -> Either [Char] Block
decodeBlock = Serialize.decode

instance ToJSON Block where
  toJSON b = object
    [ "header"       .= header b
    , "signatures"   .= Set.toList (signatures b)
    , "index"        .= index b
    , "transactions" .= transactions b
    ]

instance FromJSON Block where
  parseJSON v =
    case v of
      Object o -> do
        header       <- o .: "header"
        signatures   <- Set.fromList <$>  o .: "signatures"
        index        <- o .: "index"
        transactions <- o .: "transactions"
        pure Block {..}
      invalid -> typeMismatch "Block" v

instance ToJSON BlockHeader where
  toJSON bh = object
    [ "origin"     .= origin bh
    , "prevHash"   .= prevHash bh
    , "merkleRoot" .= decodeUtf8 (merkleRoot bh)
    , "timestamp"  .= timestamp bh
    , "consensus"  .= consensus bh
    ]

instance FromJSON BlockHeader where
  parseJSON v =
    case v of
      Object o -> do
        origin     <- o .: "origin"
        prevHash   <- o .: "prevHash"
        merkleRoot <- encodeUtf8 <$> o .: "merkleRoot"
        timestamp  <- o .: "timestamp"
        consensus  <- o .: "consensus"
        pure BlockHeader {..}
      invalid -> typeMismatch "BlockHeader" v

instance ToJSON BlockSignature where
  toJSON (BlockSignature sig addr) = object
    [ "signature"  .= toJSON (Key.encodeSig sig)
    , "signerAddr" .= addr
    ]

instance FromJSON BlockSignature where
  parseJSON = \case
    Object v -> do
      signatureTxt <- v .: "signature"
      case Encoding.parseEncodedBS (encodeUtf8 signatureTxt) of
        Left _ -> typeMismatch "Signature" (String signatureTxt)
        Right b -> case Key.decodeSig b of
          Left err -> typeMismatch "Signature" (String signatureTxt)
          Right sig -> do
            signerAddr <- v .: "signerAddr"
            pure $ BlockSignature sig signerAddr
    invalid -> typeMismatch "BlockSignature" invalid

instance Serialize BlockSignature where
  put (BlockSignature sig addr) = do
    Key.putSignature sig
    Address.putAddress addr
  get = BlockSignature <$> Key.getSignature <*> Address.getAddress

-- Necessary instances because Data.Serialize.encode/decode does not play well
-- with postgresql-simple's ByteString-to-bytea serializer
instance ToField (Set BlockSignature) where
  toField = EscapeByteA . Serialize.encode

instance FromField (Set BlockSignature) where
  fromField f mdata = do
    bs <- fromField f mdata
    case Serialize.decode <$> bs of
      Nothing                -> returnError UnexpectedNull f ""
      Just (Left err)        -> returnError ConversionFailed f err
      Just (Right blockSigs) -> return blockSigs
