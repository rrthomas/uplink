{-|

Interactive REPL for simulating script execution locally.

-}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Repl(
  -- ** REPL evaluation
  repl,
) where

import Protolude

import Control.Monad.Base

import Unsafe
import Text.Read (readMaybe)
import qualified System.Console.Haskeline as Readline

import Text.Parsec (parse)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.List as List
import qualified Data.Aeson.Encode.Pretty as A
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format

import Key
import Utils
import Script
import Storage
import Block (Block)
import Address (Address)
import Account (Account)
import Contract (Contract)

import DB
import qualified DB.LevelDB as DB (Database(..), lastBlock, loadDBs)
import qualified Time
import qualified Delta
import qualified Block
import qualified Account
import qualified Address

import qualified Script.Eval as Eval
import qualified Script.Prim as Prim
import qualified Script.Typecheck as TC
import qualified Script.Pretty as Pretty
import qualified Script.Parser as Parser
import qualified Script.Storage as SStorage

-------------------------------------------------------------------------------
-- Execution
-------------------------------------------------------------------------------

exec
  :: Script
  -> Name
  -> [Located Expr]
  -> Eval.EvalCtx
  -> Eval.EvalState
  -> IO (Either Eval.EvalFail (Value, Eval.EvalState))
exec sc@(Script _ graph ms) name args ctx state = do
  -- XXX Typecheck method args ([Located Expr])
  Eval.runEvalM ctx state $ do
   vals <- mapM Eval.evalLExpr args
   {-print vals-}
   Eval.eval sc name vals

-------------------------------------------------------------------------------
-- REPL
-------------------------------------------------------------------------------

type Repl a = Readline.InputT IO a


-- | REPL command
replCmd
  :: Eval.EvalCtx
  -> Eval.EvalState
  -> [[Char]]
  -> IO (Eval.EvalCtx, Eval.EvalState)
replCmd ctx st cmd = case cmd of

  -- Set the date context
  [":setNow", year, clock, zone] -> do
    let utcTime = year <> " " <> clock <> " " <> zone :: [Char]
    case parseDate utcTime of
      Nothing -> do
        putText "Cannot parse timestamp"
        pure (ctx, st)

      Just ts -> do
        putText $ "Setting timestamp: " <> show ts
        pure (ctx { Eval.currentTimestamp = ts } , st)

  -- Set the sender context
  [":setSender", addr] -> do
    let maddr = Address.parseAddr (BSC.pack addr)
    case maddr of
      Nothing -> do
        putText "Cannot parse address"
        pure (ctx, st)

      Just addr' -> do
        putText $ "Setting address: " <> show addr'
        pure (ctx { Eval.currentTxIssuer = addr' } , st)

  -- Show help
  [":help"] -> do
    putText ":setNow <unix timestamp>"
    putText ":setSender <addr>"
    pure (ctx, st)

  _ -> do
    putText "Unknown command"
    pure (ctx, st)

-- XXX move me somewhere more appropriate
parseDate :: [Char] -> Maybe Int64
parseDate date = do
  utc <- parseTimeM True defaultTimeLocale
                "%F %T UTC" date :: Maybe UTCTime
  pure $ round $ utcTimeToPOSIXSeconds utc

-- | REPL loop
replLoop
  :: [(Name, TC.Sig)]
  -> Script
  -> Eval.EvalCtx
  -> Eval.EvalState
  -> Bool -- verbosity
  -> IO ()
replLoop sigs script ievalCtx ievalState verbose =
    Readline.runInputT Readline.defaultSettings (loop ievalCtx ievalState)
  where
    loop :: Eval.EvalCtx -> Eval.EvalState -> Repl ()
    loop ctx evalState = do
      minput <- Readline.getInputLine "FCL> "
      case minput of
        Nothing   -> pure ()
        Just line -> do

          -- handle repl commands
          if (isPrefixOf ":" line) then do
              (ctx', state') <- liftIO $ replCmd ctx evalState (List.words line)
              loop ctx' state'

          -- handle expressions
          else do
            let ast = parse Parser.expr "<stdin>" (T.pack line)
            case ast of

              -- Parser errors
              Left err -> do
                liftIO $ print err
                loop ctx evalState
                return ()

              -- Prims
              Right (expr @ (Located loc (ECall name args)) )
                | isJust (Prim.lookupPrim name) -> do
                let res = Eval.evalLExpr expr
                val <- liftIO $ Eval.runEvalM ctx evalState res
                case val of
                  Left _ -> pass
                  Right (val', st) -> do
                    replPrint val' st verbose
                    loop ctx (st { Eval.deltas = [] })

              -- Methods
                | otherwise -> do
               let sig = filter (\(n, s) -> n == name) sigs
               if sig == []
                then do
                  putText "Not a method name"
                  loop ctx evalState
                else do
                  when verbose $ do
                    let store = Eval.tempStorage $ evalState
                    putText $ "    TempStorage = "
                    putStrLn $ Pretty.render $ SStorage.dumpStorage store

                    let store = Eval.globalStorage $ evalState
                    putText $ "    GlobalStorage = "
                    putStrLn $ Pretty.render $ SStorage.dumpStorage store

                    putText $ "    SubgraphLock = "
                    putStrLn $ Pretty.print $ fst (Eval.sideLock evalState)

                  let (n, TC.Sig ins _) = unsafeHead sig
                  if length ins /= length args
                    then do
                      putText "Wrong number of args"
                      loop ctx evalState
                    else do
                      evalState' <- replEval script name args ctx evalState verbose
                      loop ctx (evalState' { Eval.deltas = [] })

              -- Naked variables ... fail
              Right (Located loc (EVar _)) -> do
                putText "Cannot evaluate naked variable"
                loop ctx evalState

              -- Literals
              Right expr -> do
                let res = Eval.evalLExpr expr
                val <- liftIO $ Eval.runEvalM ctx evalState res
                case val of
                  Left err -> do
                    putText $ "    Error = " <> (show err)
                    loop ctx evalState

                  Right (val', st) -> do
                    replPrint val' st verbose
                    loop ctx (st { Eval.deltas = [] })

replPrint
  :: Value
  -> Eval.EvalState
  -> Bool
  -> Repl ()
replPrint val state verbose =
  if verbose then do
    putText $ "    State = "  <> toS (Pretty.print (Eval.graphState state))
    putText $ "    Deltas = "
    putStrLn $ Pretty.render $ Delta.dumpDeltas (Eval.deltas state)
    putText $ "    Return = " <> toS (Pretty.print val)
  else do
    putStrLn $ (Pretty.print val)

replEval
  :: Script
  -> Name
  -> [Located Expr]
  -> Eval.EvalCtx
  -> Eval.EvalState
  -> Bool
  -> Repl Eval.EvalState
replEval script name args evalCtx evalState verbose = do
  ret <- liftIO $ try $ exec script name args evalCtx evalState
  case ret of
    Left (FatalError msg) -> do
      print msg
      pure evalState
      -- Internal error (Haskell exception)
    Right res -> do
      case res of
        Left err -> do
          print err
          pure evalState

        Right (val, evalState') -> do
          replPrint val evalState' verbose
          pure evalState'

-------------------------------------------------------------------------------
-- Interactive Evaluation Context
-------------------------------------------------------------------------------

initEvalCtxRepl
  :: (Address, Key.PrivateKey)
  -> Block
  -> Contract
  -> IO (Either Text Eval.EvalCtx)
initEvalCtxRepl (nodeAddr, nodePrivKey) latestBlock contract = do
  let blockIdx = fromIntegral $ Block.index latestBlock
  let blockTs  = Block.timestamp $ Block.header latestBlock
  let txHash   = "REPL-EVAL-NO-TRANSACTION-HASH"          -- XXX
  let txIssuer = Address.fromRaw "THISISANINVALIDADDRESS" -- XXX
  fmap Right $
    Eval.initEvalCtx
      blockIdx
      blockTs
      nodeAddr
      txHash
      txIssuer
      nodePrivKey
      contract

-------------------------------------------------------------------------------
-- Entry
-------------------------------------------------------------------------------


-- | Interactive contract evaluation REPL
repl
  :: MonadReadDB m
  => FilePath         -- ^ Node Account filepath
  -> [(Name, TC.Sig)] -- ^ Contract signatures
  -> Script           -- ^ Contract AST
  -> Bool             -- ^ Verbosity
  -> m ()
repl accPath sigs script verbose = do
  now <- liftBase Time.now

  let accDataErrPref = "Could not read Account data from filepath supplied: "
  eAccData <- liftBase $
    first ((<>) accDataErrPref) <$>
      Account.readAccountData accPath

  let latestBlockErrPref = "Failed reading latest block from DB: "
  eLatestBlock <-
    first ((<>) latestBlockErrPref) <$> do
      eBlocks <- DB.readBlocks
      case lastMay <$> eBlocks of
        Left err         -> pure $ Left err
        Right Nothing    -> pure $ Left "No blocks in DB"
        Right (Just blk) -> pure $ Right blk

  case (,) <$> eAccData <*> eLatestBlock of
    Left err                            -> liftBase $ dieRed $ show err
    Right (nodeAccAndKeys, latestBlock) -> do

      let nodeAddr = Account.address $ fst nodeAccAndKeys
      let nodePrivKey = snd $ snd nodeAccAndKeys
      let contract = Eval.scriptToContract now nodeAddr script

      eEvalCtx <- liftBase $
        initEvalCtxRepl (nodeAddr,nodePrivKey) latestBlock contract
      eWorld   <- DB.readWorld
      case (,) <$> eEvalCtx <*> first show eWorld of
        Left err -> liftBase $ dieRed $
          "Could not initialize EvalCtx or read world from DB:\n\t" <> toS err
        Right (evalCtx,world)  -> do
          let evalState = Eval.initEvalState contract world
          liftBase $ replLoop sigs script evalCtx evalState verbose
