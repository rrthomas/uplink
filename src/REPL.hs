module REPL (
  runRepl,
) where

import Control.Distributed.Process (Process, ProcessId)
import Control.Distributed.Process.Node (LocalNode, runProcess)

import qualified Data.Aeson.Encode.Pretty as A
import Control.Distributed.Process.Lifted.Class
import Data.Maybe
import Data.Monoid
import Protolude hiding (Type)
import Console.Parser

import Script (Name(..), Type)
import Address
import Network.P2P.Simulate
import SafeString
import qualified Script.Pretty as Pretty
import System.Console.Repline
import Ledger (World(..))
import qualified Utils
import REPL.Config
import REPL.Command
import REPL.Parser
import REPL.Lexer


handleREPLInput
  :: LocalNode  -- Cloud Haskell Node to run the process on
  -> ProcessId  -- Command Process to send the message to
  -> [Char]
  -> REPLM ()
handleREPLInput node simProcId input =
  case parseREPLCmd input of
    Left  err     -> putText $ Pretty.prettyPrint err
    Right replCmd -> handleREPLCmd node simProcId replCmd

handleREPLCmd :: LocalNode -> ProcessId -> REPLCmd -> REPLM ()
handleREPLCmd node simProcId cmd = do
  simMsg <- getSimMsg cmd
  key <- gets simKey
  vbs <- gets verbose
  case simMsg of
    Nothing  -> return ()
    Just msg -> do
      handleMsg msg vbs key

      when (shouldGetCallableMethods msg) $ do
        methodsMVar <- liftIO newEmptyMVar

        updateMethodsMVar methodsMVar key

        updateREPLContext methodsMVar

  where
    handleMsg msg vbs key = liftIO $ runProcess node $ do
      handleREPLMsg simProcId msg
      -- In verbose mode, print the state and storage of the contract
      when vbs $ handleREPLMsg
        simProcId
        (QuerySimulationMsg key (QueryContract QueryContract'))

    -- | Update methodsMVar from new callable methods
    updateMethodsMVar methodsMVar key = liftIO $ runProcess node $ do
        methods <- getCallableMethods simProcId key
        liftIO $ putMVar methodsMVar methods

    -- | Update callable methods in the REPL context
    updateREPLContext methodsMVar  = do
      methods <- liftIO (takeMVar methodsMVar) >>= hoistErr
      modify $ \s -> s { callableMethods = methods }

    -- | On method calls we should update the method completion
    shouldGetCallableMethods msg = case msg of
      (UpdateSimulationMsg _ (CallMethod _)) -> True
      _ -> False


-- | Get callable methods in the current state
getCallableMethods
  :: ProcessId
  -> SimKey
  -> Process (Either Text [(Name,[(Name,Type)])])
getCallableMethods simProcId simKey = do
  methodsE <- commSimulationRemoteProc simProcId
    (QuerySimulationMsg simKey (QueryMethods QueryMethods'))
  case methodsE of
    Right (QuerySimulationSuccess (ContractMethods methods)) ->
        pure $ Right methods
    Right incorrectSuccess ->
      pure $
        Left ("Incorrect success returned when fetching callable methods:" <> formatSuccess incorrectSuccess)
    Left err -> pure $ Left (formatError err)

-- | Create simulation process and get the simulation key
createSimulation
  :: MonadProcessBase m
  => ProcessId
  -> SafeString
  -> (Address AAccount)
  -> Maybe World
  -> m (Either Text SimKey)
createSimulation simProcId scriptSS issuer mWorld = do
  simKeyE <- commSimulationRemoteProc simProcId createSimMsg
  return $ case simKeyE of
    Right (CreateSimulationSuccess (SimulationCreated simKey)) -> Right simKey
    Right other                     -> Left ("Incorrect success returned when creating contract: " <> show other)
    Left  err                       -> Left (show err)
 where
  createSimMsg = CreateSimulationMsg $ CreateSimulation issuer scriptSS mWorld

runRepl :: LocalNode -> ProcessId -> SafeString -> Maybe World -> Bool -> IO ()
runRepl node simProcId scriptSS mWorld verbose = do
  -- Ask user to provide an issuer account address
  issuer <- do
    yn <- Utils.yesOrNoPrompt
      "Do you want to provide an contract issuer address (y/n)? "
    if yn
      then do
        putText "Accounts in ledger state: "
        mapM_ (putText . toS . A.encodePretty . accounts) mWorld
        Utils.parsePrompt "Address: " addressParser
      else pure defaultAddr

  runProcess node $ do
    simKeyE <- createSimulation simProcId scriptSS issuer mWorld
    case simKeyE of
      Right simKey -> do
        putText $ "Simulation key: " <> toS (rawAddr simKey)
        methodsE <- getCallableMethods simProcId simKey
        liftIO $
          runREPLT (REPLContext simKey issuer verbose []) $
            evalRepl ">>> "
              (handleREPLInput' simKey)
              []
              Nothing
              (Word0 completer)
              (initCallableMethods methodsE)
      Left err -> liftIO $ Utils.dieRed err
 where
  handleREPLInput' simKey = unREPLM . handleREPLInput node simProcId
  defaultAddr = Address.emptyAddr
  initCallableMethods methodsE = unREPLM $ do
    methods <- hoistErr methodsE
    modify $ \s -> s { callableMethods = methods }

completer :: MonadState REPLContext m => WordCompleter m
completer n = do
  methods <- gets callableMethods
  let names = map (toS . unName . fst) methods
  return $ filter (isPrefixOf n) (names <> fmap toS reservedNames)
