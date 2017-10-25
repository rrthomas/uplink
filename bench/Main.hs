{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Protolude

import Criterion.Main

import Account
import Address
import Asset
import Block
import Config
import Contract
import Derivation
import Key
import Ledger
import SafeString
import Transaction
import Time
import Validate

import Script
import Script.Compile
import Script.Eval
import Script.Init
import Script.Storage

import Consensus.Authority.Types

import qualified Data.ByteString as BS
import System.Random


-------------------------------------------------------------------------------
-- Setup Sample contract for use in benchmarking
-------------------------------------------------------------------------------

setupSampleEvalContract :: Address -> IO (Address, Text, Script, Contract)
setupSampleEvalContract accAddr = do
  now <- Time.now
  code <- readFile "tests/sample_eval.s"
  case fmap snd (compile code) of
    Left err -> panic err
    Right ast -> do
      let globalStorage = initStorage ast
      let contractAddr = addrContract' now globalStorage
      let eContract = createContract accAddr contractAddr now code
      case eContract of
        Left err -> panic err
        Right contract -> pure (contractAddr, code, ast, contract)

-------------------------------------------------------------------------------
-- Benchmark Method Eval ('f' from tests/sample_eval.s)
-------------------------------------------------------------------------------

-- XXX Handle failures more gracefully
-- XXX Hard code script AST? currently using sample_eval.s, one method 'f'
setupMethodCallEnv :: IO (Method, EvalCtx, EvalState)
setupMethodCallEnv = do
  -- Create Keys
  (pub,priv) <- Key.new
  -- Create Account
  let acc = createAccount pub "GMT" mempty
  let accAddr = Account.address acc
  -- Add Account & contract to Ledger
  case addAccount acc genesisWorld of
    Left err -> panic $ show err
    Right accWorld -> do
      -- Create contract
      (contractAddr, _, ast, contract) <- setupSampleEvalContract accAddr
      -- Add contract to world
      case addContract contractAddr contract accWorld of
        Left err -> panic $ show err
        Right world -> do
          -- Build EvalCtx
          now <- Time.now
          evalCtx <- initEvalCtx 1 now accAddr "XXX" accAddr priv contract
          -- Build EvalState
          let evalState = initEvalState contract world
          let [f] = scriptMethods ast
          pure (f, evalCtx, evalState)

benchMethodCall :: Benchmark
benchMethodCall =
  env setupMethodCallEnv $ \ ~(f, evalCtx, evalState) ->
    bench "Eval method 'f' from sample_eval.s" $ nfIO $
      execEvalM evalCtx evalState $ void $ evalMethod f []

benchEvalArithBinOps :: Benchmark
benchEvalArithBinOps =
    env setupMethodCallEnv $ \ ~(_, evalCtx, evalState) ->
      bgroup "Benchmark 10, 100, 1000, 10000 arithmetic binary operations"
        [ benchArithOps 10 evalCtx evalState
        , benchArithOps 100 evalCtx evalState
        , benchArithOps 1000 evalCtx evalState
        , benchArithOps 10000 evalCtx evalState
        ]
  where
    benchArithOps n evalCtx evalState =
      bench ("Bench arithmetic binops: " ++ show n) $ nfIO $
        execEvalM evalCtx evalState $ void $
          evalLExpr $ lexpr n

    lexpr n = Located NoLoc $
      EAssign "x" $ mkBinOpExprs n

    mkBinOpExprs :: Int -> LExpr
    mkBinOpExprs n
      | n <= 0 = noloc $ ELit $ noloc $ LInt 7
      | otherwise = snd $
          foldr mkBinOp (0, noloc (ELit (noloc $ LInt 0))) $
            take (n - 1) args
      where
        args = map (noloc . ELit . noloc . LInt) [1..]

        noloc = Located NoLoc

        mkBinOp larg (n,rarg) =
          (n+1,) $ noloc $
            case (n `mod` 3) of
              0 -> EBinOp (noloc Add) larg rarg
              1 -> EBinOp (noloc Sub) larg rarg
              2 -> EBinOp (noloc Mul) larg rarg

-------------------------------------------------------------------------------
-- Block Env for Block benchmarks
-------------------------------------------------------------------------------

-- This benchmark creates a block with the following transactions:
-- 1)  CreateAccount  (acc1)
-- 2)  CreateAccount  (acc2)
-- 3)  CreateAccount  (acc3)
-- 4)  CreateAsset    (asset1)
-- 5)  TransferAsset  (from acc1 to acc1)
-- 6)  TransferAsset  (from acc1 to acc2)
-- 7)  TransferAsset  (from acc2 to acc3)
-- 8)  RevokeAccount  (acc2)
-- 9)  CreateContract (from sample_eval.s)
-- 10) Call           (method 'f' from sample_eval.s)

setupBlockEnv :: IO (Block, Key.PrivateKey, ApplyCtx)
setupBlockEnv = do
  keys@[(pk1,sk1),(pk2,sk2),(pk3,sk3)] <- replicateM 3 Key.new
  let pks = map fst keys
  let sks = map snd keys
  let [pk1Addr, pk2Addr, pk3Addr] = map deriveAddress pks

  -- TXs 1,2,3 (Create 3 Accounts)
  tx123 <- forM keys $ \(pk,sk) ->
    newTransaction (deriveAddress pk) Nothing sk $
      TxAccount $ CreateAccount (exportPub pk) "UTC" mempty

  -- TX 4 (Asset1 created by Acc1)
  now <- Time.now
  let asset1 = Asset "BENCH" pk1Addr now 1000 mempty Nothing Discrete
  assetSig <- Key.signS sk1 asset1
  let assetAddr1 = addrAsset assetSig asset1
  tx4 <- newTransaction pk1Addr (Just assetAddr1) sk1 $
    TxAsset $ CreateAsset "BENCH" 1000 Nothing Discrete

  -- TX 5 (Transfer from acc1 to acc1 - circulate)
  tx5 <- newTransaction pk1Addr Nothing sk1 $
    TxAsset $ Transfer assetAddr1 pk1Addr 1000

  -- TX 6 (Transfer from acc1 to acc2)
  tx6 <- newTransaction pk1Addr Nothing sk1 $
    TxAsset $ Transfer assetAddr1 pk2Addr 500

  -- TX 7 (Transfer from acc2 to acc3)
  tx7 <- newTransaction pk2Addr Nothing sk2 $
    TxAsset $ Transfer assetAddr1 pk3Addr 500

  -- TX 8 (Revoke acc2)
  tx8 <- newTransaction pk2Addr Nothing sk2 $
    TxAccount $ RevokeAccount pk2Addr

  -- TX 9 (CreateContract from sample_eval.s)
  (caddr, code, _, _) <- setupSampleEvalContract pk3Addr
  tx9 <- newTransaction pk3Addr Nothing sk3 $
    TxContract $ CreateContract caddr $ fromBytes $ toS code

  -- TX 10 (Call 'f' from sample_eval.s)
  tx10 <- newTransaction pk3Addr Nothing sk3 $
    TxContract $ Call caddr "f" []

  let txs = tx123 ++ [tx4,tx5,tx6,tx7,tx8,tx9,tx10]
  chainConf <- handleChain False "bench/chain.config"
  let poa = mkGenesisPoA chainConf
  genBlock <- genesisBlock (genesisHash chainConf) (genesisTimestamp chainConf) poa
  block <- newBlock pk1Addr (genesisHash chainConf) txs 1 sk1 poa
  let applyCtx = ApplyCtx genBlock (Block.origin $ Block.header block) sk1
  pure (block, sk1, applyCtx)

benchSignBlock :: Benchmark
benchSignBlock =
  env setupBlockEnv $ \ ~(block, sk, applyCtx) ->
    bench "Benchmark signing a block with 10 txs" $ nfIO $
      Key.sign sk (Block.hashBlock block)

benchValidateBlock :: Benchmark
benchValidateBlock =
  env setupBlockEnv $ \ ~(block, sk, applyCtx) ->
    bench "Benchmark verifying a block with 10 txs" $ nfIO $ do
      res <- Validate.verifyAndValidateBlock applyCtx genesisWorld block
      case res of
        Left err -> panic $ show err
        Right _ -> pure ()

-------------------------------------------------------------------------------
-- Benchmark Serialization
-------------------------------------------------------------------------------

setupRandomByteString :: Int -> IO ByteString
setupRandomByteString n = do
    stdgen <- getStdGen
    pure $ BS.pack $ take n $
      unfoldr (Just . randomByte) stdgen
  where
    randomByte :: StdGen -> (Word8, StdGen)
    randomByte g = first fromIntegral $ next g

benchSerializeBlock :: Benchmark
benchSerializeBlock =
  env setupBlockEnv $ \ ~(block, _, _) ->
    bench "Benchmark serialization of a block with 10txs" $
      nf encodeBlock block

setupRandomSafeString :: IO SafeString
setupRandomSafeString = fromBytes <$> setupRandomByteString maxSize

benchSerializeSafeString :: Benchmark
benchSerializeSafeString =
  env setupRandomSafeString $ \ ~ss ->
    bench "Benchmark serialization of SafeString" $
      nf encodeSafeString ss

-------------------------------------------------------------------------------
-- Benchmark for ECDSA KeyPair
-------------------------------------------------------------------------------

benchKeyGen :: Benchmark
benchKeyGen =
  bench "ECC SECP_256K1 keypair generation" $ nfIO Key.newPriv

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

benchmarks :: [Benchmark]
benchmarks =
  [ bgroup "Key benchmarks"
      [ benchKeyGen ]
  , bgroup "Script Evaluation benchmarks"
      [ benchMethodCall
      , benchEvalArithBinOps
      ]
  , bgroup "Block benchmarks"
      [ benchSignBlock
      , benchValidateBlock
      ]
  , bgroup "Serialization benchmarks"
      [ benchSerializeBlock
      , benchSerializeSafeString
      ]
  ]

main = defaultMain benchmarks
