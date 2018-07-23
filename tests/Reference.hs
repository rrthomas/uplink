{-|

Test fixtures.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Reference (

  -- ** Timestamp
  testTimestamp,
  unsafeTimestamp,

  -- ** Address
  testAddr,
  testAddr2,
  testAddr3,
  testAddrValid,

  -- ** Transaction Headers
  testTransfer,
  testCirculate,
  testCreateAccount,
  testCreateAsset,
  testRevokeAsset,
  testCreateContract,
  testRevokeAccount,
  testCall,
  testBind,
  testSyncLocal,

  -- ** Transaction
  testTx,
  testTxs,
  testInvalidTxs,

  -- ** Block
  testGenesis,
  testBlock,
  testChain,
  testBlockHash,

  -- ** Asset
  testBalance,
  testAsset1,
  testAsset2,
  testAsset3,
  testAsset3',
  testAssets,
  testAssetsDB,
  testHoldings,

  -- ** Account
  testTimezone,
  testAccount,
  testAccount2,

  -- ** Contract
  testContract,

  -- ** Storage
  testStorage,
  testGlobalStorage,
  testLocalStorage,

  -- ** Script
  defX,
  getX,
  setX,
  transX,
  testScript,
  testCode,

  -- ** Network.P2P.Message
  txMsg,
  pingMsg,
  pongMsg,
  blockMsg,
  getBlockAtIdxMsg,
  versionMsg,

  -- ** Network.P2P.Consensus
  testSignBlockMsg,
  testBlockSigMsg,

  -- ** Key
  testSig,
  testSigDecode,
  testPub,
  testPriv,
  testPub2,
  testPriv2,
  testNonce,
  testKeyExport,

  testDH,
  testDHPoint,
  testDHSecret,
  testDHSecret',

  testEncrypt,
  testDecrypt,

  testRecover,

  -- ** Consensus
  testPoA,

  -- ** TxLog
  testTxLog,

) where

import Protolude

import Unsafe (unsafeFromJust)
import System.IO.Unsafe (unsafePerformIO)

import qualified Crypto.PubKey.ECC.Types as ECC
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import qualified Crypto.PubKey.ECC.DH as DH
import qualified Crypto.Hash.MerkleTree as Merkle
import qualified Crypto.Number.Serialize as CNS

import qualified Data.ByteString as BS
import qualified Data.ByteArray as B
import qualified Data.ByteArray.Encoding as B
import qualified Data.Set as Set
import qualified Data.Serialize as S

import Account
import Address
import Asset
import Metadata
import Block
import Contract
import Fixed
import Ledger
import Hash
import Encoding
import Network.P2P.Message
import Network.P2P.Consensus as NPC
import DB
import Transaction
import Storage
import Script
import Script.Graph

import qualified SafeInteger as SI
import qualified SafeString as SS

import qualified Key
import qualified Time
import qualified Asset
import qualified Block
import qualified TxLog
import qualified Delta
import qualified Script
import qualified Address
import qualified Account
import qualified SafeString
import qualified Transaction
import qualified Data.Map as Map
import qualified Network.P2P.Consensus as NPC
import qualified Network.P2P.Message as NPM
import qualified Network.P2P.Service as Service
import qualified Script.Graph as Graph
import qualified Script.Prim as Prim

import Consensus.Authority.Params as CAP

import System.FilePath

-------------------------------------------------------------------------------
-- Timestamp Fixtures
-------------------------------------------------------------------------------

testTimestamp :: Time.Timestamp
testTimestamp = 1231006505

{-# NOINLINE unsafeTimestamp #-}
unsafeTimestamp :: Time.Timestamp
unsafeTimestamp = unsafePerformIO Time.now

-------------------------------------------------------------------------------
-- Address Fixtures
-------------------------------------------------------------------------------

testAddr :: Address a
testAddr = deriveAddress testPub

testAddr2 :: Address a
testAddr2 = deriveAddress testPub2

testAddr3 :: Address a
testAddr3 = deriveAddress testPub3

testHolder :: Holder
testHolder = Holder (testAddr :: Address AAccount)

testHolder2 :: Holder
testHolder2 = Holder (testAddr2 :: Address AAccount)

testAddrValid :: IO ()
testAddrValid = do
  (pub, addr) <- newPair
  print $ verifyAddress pub addr

-------------------------------------------------------------------------------
-- Transaction Header Fixtures
-------------------------------------------------------------------------------

-- XXX Make test tx headers for all tx header types

assetAddr_, toAddr_ :: Address a
assetAddr_ = Address.fromRaw "43WRxMNcnYgZFcE36iohqrXKQdajUdAxeSn9mzE1ZedB"
toAddr_ = Address.fromRaw "7mR5d7s6cKB4qjuX1kiwwNtygfURhFQ9TKvEd9kmq6QL"

toHolder_ :: Holder
toHolder_ = Holder (toAddr_ :: Address AAccount)

testArgs :: [Value]
testArgs = [
    (VInt 1)
  , (VFloat 3.5)
  , (VBool True)
  , (VContract testAddr)
  , (VMsg "Hello world")
  , VVoid
  , VUndefined
  ]

testCirculate :: TransactionHeader
testCirculate = TxAsset Transfer {
    assetAddr = Reference.assetAddr_
  , toAddr    = Reference.toHolder_
  , balance   = 1
  }

testTransfer :: TransactionHeader
testTransfer = TxAsset Transfer {
    assetAddr = Reference.assetAddr_
  , toAddr    = Reference.toHolder_
  , balance   = 1
  }

testCreateAccount :: TransactionHeader
testCreateAccount = TxAccount CreateAccount {
    pubKey   = SafeString.fromBytes' $ Key.unHexPub (Key.encodeHexPub testPub)
  , timezone = "GMT"
  , metadata = mempty
  }

testCreateAsset :: TransactionHeader
testCreateAsset = TxAsset CreateAsset {
    assetName = "test"
  , supply    = 1000
  , reference = Just Asset.Token
  , assetType = Asset.Discrete
  , metadata = testMetadata
  }

testRevokeAsset :: TransactionHeader
testRevokeAsset = TxAsset $ RevokeAsset testAddr

testCreateContract :: TransactionHeader
testCreateContract = TxContract CreateContract {
    contract = SafeString.fromBytes' $ toS testCode
  }

testRevokeAccount :: TransactionHeader
testRevokeAccount = TxAccount RevokeAccount {
    address = testAddr
  }

testCall :: TransactionHeader
testCall = TxContract Call {
    address = testAddr
  , method  = "get"
  , args    = []
  }

testBind :: TransactionHeader
testBind = TxAsset Bind {
    assetAddr    = Reference.assetAddr_
  , contractAddr = Reference.toAddr_
  , bindProof    = bimap SI.toSafeInteger' SI.toSafeInteger' $
                     Key.getSignatureRS testSig
  }

testSyncLocal :: TransactionHeader
testSyncLocal = TxContract $
  SyncLocal testAddr (InitialCommit (SI.toSafeInteger' 1, SI.toSafeInteger' 2))

-------------------------------------------------------------------------------
-- Transaction Fixtures
-------------------------------------------------------------------------------

testTx :: TransactionHeader -> Transaction
testTx hdr = tx
  where
    Just sig = Key.signSWith testPriv testNonce hdr
    tx = Transaction {
      header    = hdr
    , origin    = testAddr
    , signature = Key.encodeSig sig
    }

testTx_ :: TransactionHeader -> IO Transaction
testTx_ hdr = mkTx <$> Key.signS testPriv hdr
  where
    mkTx sig = Transaction {
      header    = hdr
    , origin    = testAddr
    , signature = Key.encodeSig sig
    }


testTxs :: [Transaction]
testTxs = map testTx
  [ testCreateAccount
  , testCreateAsset
  , testTransfer
  , testCreateContract
  , testRevokeAccount
  , testCall
  , testBind
  , testSyncLocal
  ]

testInvalidTxs :: [InvalidTransaction]
testInvalidTxs =
    map (uncurry InvalidTransaction . first testTx) itxs
  where
    itxs =
      [ (testCreateAccount
        , itxhdr $ InvalidTxAccount (InvalidPubKeyByteString "thisisnotapublickey")
        )
      , (testTransfer
        , itxhdr $ InvalidTxAsset $ Transaction.AssetError (ReceiverDoesNotExist testHolder)
        )
      , (testCreateContract
        , itxhdr $ InvalidTxContract (InvalidContract "this is not a valid script")
        )
      , (testRevokeAccount
        , itxhdr $ InvalidTxAccount (RevokeValidatorError testAddr2)
        )
      , (testCall
        , InvalidPubKey
        )
      , (testBind
        , InvalidTxField (InvalidTxTimestamp testTimestamp)
        )
      , (testSyncLocal
        , NoSuchOriginAccount testAddr
        )
      ]

    itxhdr = InvalidTxHeader


-------------------------------------------------------------------------------
-- Block Fixtures
-------------------------------------------------------------------------------

testGenesis :: IO Block
testGenesis = do
  let
    txs       = []
    ts        = testTimestamp
    origin    = testAddr
    priv      = testPriv
    index     = 0

    header = BlockHeader {
      origin     = origin
    , prevHash   = Hash.emptyHash
    , merkleRoot = Merkle.mtHash (Merkle.mkMerkleTree txs)
    , timestamp  = ts
    , consensus  = testPoA
    }

    sig = unsafeFromJust $ Key.signSWith priv testNonce header
    blockSig = BlockSignature sig (Address.deriveAddress $ Key.toPublic priv)

  return $ Block.Block {
    header       = header
  , index        = index
  , signatures   = Set.singleton blockSig
  , transactions = txs
  }

-- | Test block signed with test key
testBlock :: Block -> [Transaction] -> IO Block
testBlock lastBlock txs = do
  newBlock
    testAddr
    (hashBlock lastBlock)
    txs
    (Block.index lastBlock + 1)
    testPriv
    testPoA

-- | Construct a test chain signed wdith test key
testChain :: IO [Block]
testChain = do
  block0 <- genesisBlock "83cb2aef3" testTimestamp testPoA
  block1 <- testBlock block0 []
  block2 <- testBlock block1 []
  block3 <- testBlock block2 []
  return [block0, block1, block2, block3]

testBlockHash :: IO (Hash Base16ByteString)
testBlockHash = do
  genesisBlock <- genesisBlock "29c9abd5" testTimestamp testPoA
  return $ hashBlock genesisBlock

-------------------------------------------------------------------------------
-- Asset Fixtures
-------------------------------------------------------------------------------

testBalance :: Text
testBalance = displayType (Fractional Prec6) 42

mkTestAsset
  :: Text
  -> Asset.Balance
  -> Asset.Ref
  -> Asset.AssetType
  -> Asset
mkTestAsset name supply ref typ = Asset{..}
  where
    issuedOn = testTimestamp
    issuer = testAddr
    holdings = mempty
    assetType = typ
    reference = Just ref
    metadata = testMetadata

    address =
      transactionToAddress $
        testTx testCreateAsset

testAsset1 :: Asset
testAsset1 =
    mkTestAsset name supply reference assetType
  where
    name = "hamburgers"
    supply = 100000
    assetType = Discrete
    reference = Security

testAsset2 :: Asset
testAsset2 =
    mkTestAsset name supply reference assetType
  where
    name = "usd"
    supply = 100
    assetType = Fractional Prec2
    reference = USD

-- | A test asset with holdings!
testAsset3 :: Asset
testAsset3 =
    mkTestAsset name supply reference assetType
  where
    name = "rights"
    supply = 50
    assetType = Binary
    reference = Token

testAsset3' :: Asset
testAsset3' =
  let holdings = Holdings $
        Map.fromList [(testHolder, 10000),(testHolder, 10000)]
  in testAsset3 {holdings = holdings, supply = 10000000}

-- | Warning: All assets will have the same address
testAssets :: [Asset]
testAssets =
  [testAsset1, testAsset2, testAsset3]

-- | Create test assets with random addresses
testAssetsDB :: IO [Asset]
testAssetsDB = do
    addr1 <- randAddr
    addr2 <- randAddr
    addr3 <- randAddr
    pure $
      [ testAsset1 { Asset.address = addr1 }
      , testAsset2 { Asset.address = addr2 }
      , testAsset3 { Asset.address = addr3 }
      ]
  where
    randAddr = do
      pub <- fst <$> Key.new
      pure $ deriveAddress pub

testHoldings :: IO (Either Asset.AssetError Asset)
testHoldings = do
  a1 <- newHolder
  a2 <- newHolder

  let x = testAsset1
  let transaction = pure testAsset1 >>=
        circulateSupply a1 1000 >>= circulateSupply a2 1000
  return transaction
  where
    newHolder :: IO Holder
    newHolder = do
      a <- Address.newAddr
      return $ Holder (a :: Address AAccount)

-------------------------------------------------------------------------------
-- Account
-------------------------------------------------------------------------------

testTimezone :: ByteString
testTimezone = "GMT"

testAccount :: Account
testAccount = Account
  { publicKey   = testPub
  , address     = Address.deriveAddress testPub
  , timezone    = "America/New_York"
  , metadata    = Metadata $
      Map.fromList [ ("Company", "Adjoint Inc.") ]
  }

testAccount2 :: Account
testAccount2 = Account
  { publicKey   = testPub2
  , address     = Address.deriveAddress testPub2
  , timezone    = "America/Boston"
  , metadata    = testMetadata
  }

-------------------------------------------------------------------------------
-- Metadata
-------------------------------------------------------------------------------
testMetadata = Metadata $
  Map.fromList [ ("Company", "Adjoint Inc.") ]


-------------------------------------------------------------------------------
-- Contract
-------------------------------------------------------------------------------

testContract :: Time.Timestamp -> Contract
testContract now = Contract {
    timestamp     = now
  , script        = testScript
  , globalStorage = testGlobalStorage
  , localStorage  = mempty
  , localStorageVars = mempty
  , methods       = Script.methodNames testScript
  , state         = Graph.GraphInitial
  , address       = testAddr
  , owner         = testAddr
  }

------------------------------------------------------------------------------------
-- Storage
-------------------------------------------------------------------------------

-- TODO All the the Value types
testStorage :: Storage
testStorage = Map.fromList [
    ("a", VInt 3)
  , ("b", VFloat 3.14)
  , ("c", VBool True)
  , ("d", VAccount testAddr)
  , ("e", VVoid)
  , ("g", VEnum (EnumConstr "Foo"))
  , ("h", VAsset testAddr2)
  , ("i", VContract testAddr)
  ]

testGlobalStorage :: GlobalStorage
testGlobalStorage = GlobalStorage testStorage

-- XXX Local storage should be empty until we figure out what to do with it...
testLocalStorage :: LocalStorage
testLocalStorage = LocalStorage mempty

-------------------------------------------------------------------------------
-- Script
-------------------------------------------------------------------------------

testLocated :: a -> Located a
testLocated = Located NoLoc

enumE :: EnumDef
enumE = EnumDef (testLocated $ "E") [ testLocated $ EnumConstr "Foo"
                                    , testLocated $ EnumConstr "Bar"
                                    ]

defX :: Def
defX = GlobalDef TInt RoleAny "x" . testLocated . ELit . testLocated $ LInt 0

defY :: Def
defY = GlobalDef (TEnum "E")
                 RoleAny
                 "y"
                 (testLocated . ELit . testLocated . LConstr . EnumConstr $ "Foo")

defM :: Def
defM = GlobalDef (TColl (TMap TAccount (TEnum "E"))) RoleAny "m" (testLocated . ELit . testLocated $ LMap mempty)

setY :: Method
setY = Method (Main "initial") RoleAny "setY" [] $ eseq NoLoc $
  [ testLocated $ EAssign "y" (testLocated $ ELit $ testLocated (LConstr . EnumConstr $ "Bar"))
  , testLocated $ ECall (Left Prim.Transition) [(testLocated $ ELit $ testLocated (LState "set"))]
  ]

setX :: Method
setX = Method (Main "set") RoleAny "setX" [] $ eseq NoLoc $
  [ testLocated $ EAssign "x" (testLocated $ ELit $ testLocated (LInt 42))
  , testLocated $ ECall (Left Prim.Transition) [(testLocated $ ELit $ testLocated (LState "get"))]
  ]

getX :: Method
getX = Method (Main "get") RoleAny "getX" [] $ eseq NoLoc $
  [ testLocated $ ECall (Left Prim.Transition) [(testLocated $ ELit $ testLocated (LState "terminal"))]]

transX :: [Transition]
transX = [
    Arrow Initial (Step "set")
  , Arrow (Step "set") (Step "get")
  , Arrow (Step "get") Terminal
  ]

helperInsert :: Helper
helperInsert = Helper (testLocated "insertFoo") [Arg TAccount (testLocated "a")] $ eseq NoLoc $
  [ testLocated $ ECall (Left (Prim.MapPrimOp Prim.MapInsert)) (map testLocated [EVar (testLocated "a"), ELit (testLocated (LConstr (EnumConstr "Foo"))), EVar (testLocated "m")]) ]

testScript :: Script
testScript = Script [enumE] [defX, defY, defM] transX [setY, getX, setX] [helperInsert]

testCode :: Text
testCode =
  " global float x = 0.0; \
  \ global fixed3 f3 = 1.234f; \
  \ global fixed2 q; \
  \ local int y = 7; \
  \ local float v; \
  \ assetFrac5 z = a'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65'; \
  \ contract c = c'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65';  \
  \ account a = u'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65'; \
  \  \
  \ datetime dt; \
  \  \
  \ transition initial -> setX; \
  \ transition setX -> update; \
  \ transition update -> setX; \
  \ transition setX -> setup; \
  \ transition update -> setup; \
  \ transition setup -> confirmation; \
  \ transition confirmation -> settlement; \
  \ transition settlement -> terminal; \
  \  \
  \ transition initial -> circulated; \
  \ transition circulated -> terminal; \
  \  \
  \ @setDate \
  \ setDate() { \
  \   dt = \"2020-10-20T15:50:12+00:00\"; \
  \ } \
  \  \
  \ @initial \
  \ initialize () { \
  \   transitionTo(:setX); \
  \ } \
  \  \
  \ @setup \
  \ confirm () { \
  \   transitionTo(:confirmation); \
  \ } \
  \  \
  \ @confirmation \
  \ settle () { \
  \   transitionTo(:settlement); \
  \ } \
  \  \
  \ @settlement \
  \ finalize () { \
  \   transitionTo(:terminal); \
  \ } \
  \  \
  \ @setX \
  \ setX (int j, float k) { \
  \   x = k; \
  \   y = y * j; \
  \   f3 = 2.516f + f3; \
  \   x = fixed3ToFloat(floatToFixed3(k)) + x; \
  \   transitionTo(:update); \
  \ } \
  \  \
  \ @setX \
  \ fixX () { \
  \   transitionTo(:setup); \
  \ } \
  \  \
  \ @update \
  \ fixY () { \
  \   transitionTo(:setup); \
  \ } \
  \  \
  \ @update \
  \ update () { \
  \   j = 10 + 7 * 10; \
  \   k = j; \
  \   l = k; \
  \   m = 1.23f + 4.56f - 7.89f * 9.87f / 65.43f; \
  \   q = m + 1.00f + floatToFixed2(x); \
  \   transitionTo(:setX); \
  \ } \
  \  \
  \ @f \
  \ f (int j, bool k) {  \
  \   if (k) { \
  \   } else { \
  \   }; \
  \ } \
  \  \
  \  \
  \  \
  \ @g \
  \ g (assetDisc f, account t) { \
  \   if (assetExists(f) && accountExists(t)) { \
  \     transferTo(f, 20); \
  \     transferFrom(f, 20, t); \
  \   }; \
  \ } \
  \  \
  \ @initial \
  \ circulate(assetFrac2 af2, fixed2 amount) { \
  \   circulate(af2, amount); \
  \   transitionTo(:circulated); \
  \ } \
  \  \
  \ @circulated \
  \ transfer(assetBin ab, account from, account to, bool amount) { \
  \   transferHoldings(from,ab,amount,to); \
  \   terminate(\"finished transfer\"); \
  \ }"

-------------------------------------------------------------------------------
-- Network.P2P.Message Fixtures (Payloads) -- XXX Remove IO from all of these
-------------------------------------------------------------------------------

txMsg :: IO Message
txMsg = do
  let hdr = testCreateAccount
  let tx = testTx hdr
  let msg = NPM.SendTransactionMsg tx
  pure (NPM.SendTx msg)

pingMsg :: IO Message
pingMsg = pure (NPM.Ping "")

pongMsg :: IO Message
pongMsg = pure (NPM.Pong "")

versionMsg :: IO Message
versionMsg = pure (NPM.Version NPM.version)

getBlockAtIdxMsg :: IO Message
getBlockAtIdxMsg = do
  let msg = NPM.GetBlockMsg 1 "nid://127.0.1.1:8001:0"
  pure (NPM.GetBlock msg)

blockMsg :: IO Message
blockMsg = do
  blk <- testGenesis
  let msg = NPM.BlockMsg blk "XXX"
  pure (NPM.Block msg)

-------------------------------------------------------------------------------
-- Network.P2P.Consensus Fixtures
-------------------------------------------------------------------------------

testSignBlockMsg :: IO NPC.SignBlockMsg
testSignBlockMsg = do
  (acc,(_,sk)) <- Account.newAccount "GMT" mempty
  let accAddr = Account.address acc
  pbHash <- testBlockHash
  let tx1 = testTx testCreateAccount
  let tx2 = testTx testCreateAccount
  block <- Block.newBlock accAddr pbHash [tx1,tx2] 1337 sk testPoA
  return $ NPC.SignBlockMsg block

testBlockSigMsg :: IO NPC.BlockSigMsg
testBlockSigMsg = do
  (acc,(_,sk)) <- Account.newAccount "GMT" mempty
  let accAddr = Account.address acc
  pbHash <- testBlockHash
  let tx1 = testTx testCreateAccount
  let tx2 = testTx testCreateAccount
  block <- Block.newBlock accAddr pbHash [tx1,tx2] 1337 sk testPoA
  blockSig' <- Key.sign sk (Hash.getRawHash $ Block.hashBlock block)
  let blockSig = Block.BlockSignature blockSig' accAddr
  return $ NPC.BlockSigMsg blockSig

-------------------------------------------------------------------------------
-- Key
-------------------------------------------------------------------------------

testSigDecode :: IO Bool
testSigDecode = do
  (pk1, sk1) <- Key.new
  sig <- Key.sign sk1 "foo"
  let sigE = Key.encodeSig sig
  let Right sigD = Key.decodeSig sigE
  return $ sig == sigD

testDH :: IO (Key.PubKey, Key.PubKey, Bool)
testDH = do
  (priv1, pub1) <- Key.dhGenerateKeyPair -- client
  (priv2, pub2) <- Key.dhGenerateKeyPair -- server
  let Just s1 = Key.dhGetShared priv1 pub2
  let Just s2 = Key.dhGetShared priv2 pub1

  let sec1 = Key.secretToPrivate s1
  let sec2 = Key.secretToPrivate s2
  print (sec1, sec2)

  return (pub1, pub2, sec1 == sec2)

testRecover :: IO Bool
testRecover = do
  let k    = 101
  let Just sig = Key.signWith testPriv k testMsg
  let (rpub1, rpub2) = Key.recover sig testMsg
  return $ testPub == rpub1 || testPub == rpub2

testMsg :: ByteString
testMsg = "The quick brown fox jumped over the lazy dog."

testEncrypt  :: IO ByteString
testEncrypt = do
  let Just (DH.SharedKey key) = Key.dhGetShared testPriv testPub2
  print (B.unpack key)
  Key.encrypt key testMsg

testDecrypt :: IO (Maybe ByteString)
testDecrypt = do
  let Just (DH.SharedKey key) = Key.dhGetShared testPriv testPub2
  ciphertext <- Key.encrypt key testMsg
  print (B.unpack key)
  print ciphertext
  Key.decrypt key ciphertext

testDHPoint :: ECDSA.PublicPoint
testDHPoint = DH.calculatePublic Key.sec_p256k1 (ECDSA.private_d testPriv)

testDHSecret :: ByteString
testDHSecret = B.convert $ DH.getShared Key.sec_p256k1 (ECDSA.private_d testPriv) testDHPoint

testDHSecret' :: Integer
testDHSecret' = CNS.os2ip $ DH.getShared Key.sec_p256k1 (ECDSA.private_d testPriv) testDHPoint

testKeyExport :: IO ()
testKeyExport = do
  (pub, priv) <- Key.new
  let pem = Key.exportPriv priv
  let Right (pub', priv') = Key.importPriv pem
  putStrLn pem
  print (pub==pub', priv==priv')

  let pem = Key.exportPub pub
  let Right pub'' = Key.importPub pem
  putStrLn pem
  print (pub==pub'')

-- Warning: If you change this, most of the tx serialization tests will fail;
-- The nonce is used to sign the transaction header, included in the tx body.
testNonce :: Integer
testNonce = 42

testSig :: ECDSA.Signature
testSig = ECDSA.Signature
  115136800820456833737994126771386015026287095034625623644186278108926690779567
  98245280522003505644797670843107276132602050133082625768706491602875725788467

-- | Test public key
testPub :: Key.PubKey
testPub = Key.PubKey
  ECDSA.PublicKey
    { public_curve = Key.sec_p256k1
    , public_q =
        ECC.Point
          79174541683660805620639640382768661759397823295690888507762753093299621916987
          71065057682299419085325030940046895916159750330092956837115984315857371796477
    }

-- | Test private key
testPriv :: ECDSA.PrivateKey
testPriv =
  ECDSA.PrivateKey
    { ECDSA.private_curve = Key.sec_p256k1
    , private_d =
        72637887363324669071595225655990695893413546682343152974463667925881860469868
    }

testPub2 :: Key.PubKey
testPub2 = Key.PubKey
  ECDSA.PublicKey
    { public_curve = Key.sec_p256k1,
      public_q =
        ECC.Point
          1214472788908201963423194854954899474972637934119143134374611198349064747631
          55018619338720117837520711191755301992479046934581364159114240410943276116468
    }

testPub3 :: Key.PubKey
testPub3= Key.PubKey
  ECDSA.PublicKey
    { public_curve = Key.sec_p256k1
    , public_q =
        ECC.Point
          33718916237022633221230485306632773657519572332333597686012127480807130421976
          58146723934287926414800677754909247104297877689488401591859022271503476599055
    }

testPriv2 :: ECDSA.PrivateKey
testPriv2 =
  ECDSA.PrivateKey
    { ECDSA.private_curve = Key.sec_p256k1
    , private_d =
        63397430372085899480856522915791132982175202462259856053849906393820683762766
    }

-------------------------------------------------------------------------------
-- Consensus
-------------------------------------------------------------------------------

testPoA :: CAP.PoA
testPoA = CAP.PoA
  { validatorSet  = mempty
  , blockPeriod   = 1
  , blockGenLimit = 1
  , signerLimit   = 1
  , threshold     = 1
  , minTxs        = 1
  }

-------------------------------------------------------------------------------
-- TxLog
-------------------------------------------------------------------------------

testTxLog :: IO ()
testTxLog = do
  let dt1 = Delta.ModifyGlobal "a" (VFloat 3.14)
  let dt2 = Delta.ModifyGlobal "b" (VInt 3)
  TxLog.writeDeltas (TxLog.txLogFile "node1") 0 testAddr [dt1, dt2]
