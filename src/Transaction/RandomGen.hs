{-# LANGUAGE ScopedTypeVariables #-}

module Transaction.RandomGen where

import Protolude
import Data.List
import qualified Data.Map as Map
import System.Random (randomRIO)
import qualified Crypto.PubKey.ECC.Types as ECC
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import Crypto.PubKey.ECC.Generate (generate)
import Crypto.Random.Types (MonadRandom(..))

import qualified Ledger
import qualified Asset
import qualified Metadata as M
import qualified Transaction as Tx
import Address
import qualified SafeString
import qualified Contract
import qualified Script
import qualified Key
import Data.Serialize as S
import Transaction.Generics (TxHeader(..))

data TxAssetGen
  = CreateTxAsset
  | CirculateTxAsset [(Address AAsset, Asset.Balance)]
  | TransferTxAsset [(Address AAsset, [Address AAccount], Asset.Balance)]

data TxAccountGen
  = CreateTxAccount
  | RevokeTxAccount [Address AAccount]

data TxContractGen
  = CreateTxContract
  | CallTxContract [(Address AContract, Contract.Contract)]

-- | Generate a valid random transaction
genRandTransaction
  :: Address AAccount
  -> Key.PrivateKey
  -> Ledger.World
  -> IO Tx.Transaction
genRandTransaction addr privKey world = do
  let txElems = tHeader (witness :: Tx.TransactionHeader)
  txElem <- rndItem txElems
  txHeader <- case Tx.txElemToTransactionHeader txElem of
    Tx.TxContract _ -> do
      let txContractGen = getValidTxContracts world
      createTxContract txContractGen

    Tx.TxAccount _ -> do
      let txAccountGen = getValidTxAccounts world
      createTxAccount txAccountGen

    Tx.TxAsset _ -> do
      let txAssetGen = getValidTxAssets addr world
      createTxAsset addr txAssetGen

  sig <- Key.sign privKey $ S.encode txHeader
  pure $ Tx.Transaction txHeader (Key.encodeSig sig) addr


-----------------------------------------------------
-- TxContract random generators
-----------------------------------------------------

createTxContract :: [TxContractGen] -> IO Tx.TransactionHeader
createTxContract txTypes = do
  txType <- rndItem txTypes
  case txType of
    CreateTxContract ->
      pure $ Tx.TxContract Tx.CreateContract {
        contract = SafeString.fromBytes' $ toS testCode
      }
    CallTxContract contracts -> do
      (addr, contract) <- rndItem contracts
      method <- rndItem (Contract.callableMethods contract)
      let methodName = toS $ Script.unName $ Script.methodName method
      pure $ Tx.TxContract $ Tx.Call addr methodName []
    -- TODO: SyncLocal?

getValidTxContracts :: Ledger.World -> [TxContractGen]
getValidTxContracts world =
  catMaybes
    [ Just CreateTxContract
    , mCallTxContract
    ]
  where
    mCallTxContract = case contracts of
      [] -> Nothing
      cs -> Just (CallTxContract cs)
    contracts = Map.toList (Ledger.contracts world)


-----------------------------------------------------
-- TxAccount random generators
-----------------------------------------------------

createTxAccount :: [TxAccountGen] -> IO Tx.TransactionHeader
createTxAccount txTypes = do
  txType <- rndItem txTypes
  case txType of
    CreateTxAccount -> do
      (pubKey', privKey) <- generateKeys Key.sec_p256k1
      let pubKey = SafeString.fromBytes' $ Key.unHexPub (Key.encodeHexPub pubKey')
      timezone <- rndItem timezones
      pure $ Tx.TxAccount $ Tx.CreateAccount pubKey timezone mempty

    RevokeTxAccount addrs ->
      Tx.TxAccount . Tx.RevokeAccount <$> rndItem addrs

getValidTxAccounts :: Ledger.World -> [TxAccountGen]
getValidTxAccounts world =
  catMaybes
    [ Just CreateTxAccount
    , mRevokeAccount
    ]
  where
    mRevokeAccount = case accounts of
      [] -> Nothing
      xs -> Just (RevokeTxAccount xs)
    accounts = Map.keys (Ledger.accounts world)

-----------------------------------------------------
-- TxAsset random generators
-----------------------------------------------------

-- | Create a transaction asset from valid assets
-- taking the ledger into account
createTxAsset
  :: Address AAccount
  -> [TxAssetGen]
  -> IO Tx.TransactionHeader
createTxAsset nodeAddr txTypes = do
  txType <- rndItem txTypes
  case txType of
    CreateTxAsset -> do
      rndSupply <- randomRIO (1, 5000)
      rndAssetName <- replicateM 10 (randomRIO ('a', 'z'))
      pure $ Tx.TxAsset Tx.CreateAsset {
          assetName = SafeString.fromBytes' $ toS rndAssetName
        , supply    = rndSupply
        , reference = Just Asset.Token
        , assetType = Asset.Discrete
        , metadata = M.Metadata $
            Map.fromList [ ("Company", "Adjoint Inc.") ]
      }

    CirculateTxAsset xs -> do
      (assetAddr, maxBalance) <- rndItem xs
      balance <- randomRIO (0, maxBalance)
      pure $ Tx.TxAsset $ Tx.Circulate assetAddr balance

    TransferTxAsset xs -> do
      (assetAddr, toAccounts, maxBalance) <- rndItem xs
      toAcc <- rndItem toAccounts
      balance <- randomRIO (1, maxBalance)
      pure $ Tx.TxAsset $ Tx.Transfer assetAddr (Asset.Holder toAcc) balance

    -- TODO: RevokeAsset?
    -- TODO: Bind?

-- | Get valid transaction assets from the Ledger state
getValidTxAssets :: Address AAccount -> Ledger.World -> [TxAssetGen]
getValidTxAssets nodeAddr world =
  catMaybes
    [ Just CreateTxAsset
    , mCirculate
    , mTransfer
    ]
  where
    mTransfer =
      case validAssets of
        [] -> Nothing
        as -> Just (TransferTxAsset as)
      where
        validAssets =
          mapMaybe transferTxAssetData $
            toList (Ledger.assets world)

        transferTxAssetData a
          | (issuerHolder a `elem ` holders a)
            && (Asset.holdings a /= mempty) =
              case issuerBalance a of
                Nothing -> Nothing
                Just balance ->
                  Just (Asset.address a, otherAccounts a, balance)
          | otherwise = Nothing

        otherAccounts a = Map.keys $
          Map.delete (Asset.issuer a) (Ledger.accounts world)
        holders a = Map.keys $ Asset.unHoldings (Asset.holdings a)
        issuerHolder a = Asset.Holder (Asset.issuer a)
        issuerBalance a = Map.lookup (issuerHolder a) $
          Asset.unHoldings (Asset.holdings a)

    mCirculate =
      case validAssets of
        [] -> Nothing
        as -> Just (CirculateTxAsset as)
      where
        validAssets =
          mapMaybe circulateTxAssetData $
            toList (Ledger.assets world)

        circulateTxAssetData a
          | (Asset.issuer a == nodeAddr) && (Asset.supply a > 0) =
              Just (Asset.address a, Asset.supply a)
          | otherwise = Nothing

----------------------------------
-- Utils
-- -------------------------------

generateKeys :: MonadRandom m => ECC.Curve -> m (Key.PubKey, ECDSA.PrivateKey)
generateKeys curve = do
    (pubKey, privKey) <- generate curve
    pure (Key.PubKey pubKey, privKey)

timezones :: [SafeString.SafeString]
timezones =
  [ "GMT"
  , "America/New_York"
  , "America/Boston"
  , "UTC"
  ]

rndItem :: [a] -> IO a
rndItem xs = do
  idx <- randomRIO (0, length xs - 1)
  pure $ xs !! idx


testCode :: Text
testCode =
  " global float x = 0.0; \
  \ global fixed3 f = 1.234f; \
  \ global fixed2 q; \
  \ local int y = 7; \
  \ local float v; \
  \ assetFrac5 z = 'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65'; \
  \ contract c = 'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65';  \
  \ account a = 'H1tbrEKWGpbPjSeG856kz2DjViCwMU3qTw3i1PqCLz65'; \
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
  \   f = 2.516f + f; \
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
  \ } "

