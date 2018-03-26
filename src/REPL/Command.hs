{-# LANGUAGE MultiWayIf #-}

module REPL.Command (
  REPLCmd(..),

  getSimMsg,
  handleREPLMsg,
  hoistErr,
  formatError,
  formatSuccess
) where
import Protolude

import Control.Distributed.Process (Process, ProcessId)
import Control.Distributed.Process.Node (LocalNode, runProcess)
import qualified Data.Aeson.Encode.Pretty as A
import Data.Text as T
import Data.Monoid
import System.Console.Repline (abort)

import qualified Datetime as D
import qualified Datetime.Types as D
import qualified Contract
import qualified Utils
import qualified Script.Storage as SStorage
import qualified Storage

import qualified Network.P2P.Simulate as Sim
import Network.P2P.Simulate (SimulationMsg(..), commSimulationRemoteProc, QuerySimulationSuccess(..), SimulationSuccess(..) )
import Address
import Script.Parser (contents, mkParseErrInfo, ParseErrInfo, textLit)
import REPL.Lexer
import REPL.Config
import Script      (Script, Value, Name, Method(..), Arg, Type, TimeDelta(..), argtys, createEnumInfo, scriptEnums)
import Script.Pretty (ppr, prettyPrint, (<+>), Doc, punctuate, hsep)

data REPLCmd =
    SetSender Address
  | SetTimestamp D.Datetime
  | CallContract Name [Value]
  | Query Sim.QuerySimulation
  | Help
  | Quit


handleREPLMsg :: ProcessId -> SimulationMsg -> Process ()
handleREPLMsg simProc simMsg = do
  res <- commSimulationRemoteProc simProc simMsg
  putText $
   either formatError formatSuccess res


formatSuccess :: SimulationSuccess -> Text
formatSuccess simSuccess = case simSuccess of
  (QuerySimulationSuccess querySuccess) -> case querySuccess of
    (LedgerState world) -> toS $ A.encodePretty world
    (Contract c) -> T.unlines [
        "State: " <> (toS $ A.encodePretty $ Contract.state c)
      , "Global Storage:"
      , toS $ prettyPrint $ ppStorage $ Storage.unGlobalStorage $ Contract.globalStorage c
      -- TODO print local storage
      ]
      where
        ppStorage = SStorage.dumpStorage tmpEnumInfo
        tmpEnumInfo = createEnumInfo $ Script.scriptEnums $ Contract.script c
    (SimulationAsset asset) -> toS $ A.encodePretty asset
    (SimulationAssets assets) -> T.unlines [
        "Assets:"
      , toS $ A.encodePretty assets
      ]
    (ContractMethods methods) -> T.unlines
        ("Methods:" : (fmap (prettyPrint . ppMethod) methods))
      where
        ppMethod :: (Name, [(Name, Script.Type)]) -> Doc
        ppMethod (name, args) =
          ppr name <+> ":" <+> (hsep $ (punctuate  "-> " (fmap (\(_, typ) -> ppr typ) args)) <> [ppr ("-> ()":: Text)])

  _ -> show simSuccess

formatError :: Sim.SimulationError -> Text
formatError simErr = case simErr of
  Sim.UpdateSimulationError (Sim.CallMethodError err) -> case err of
    (Sim.EvalFail          e) -> prettyPrint e
    (Sim.InvalidMethodName e) -> prettyPrint e
    (Sim.InvalidArgType    e) -> prettyPrint e
  _ -> show simErr

getSimMsg :: REPLCmd -> REPLM (Maybe Sim.SimulationMsg)
getSimMsg cmd = do
  key <- gets simKey
  case cmd of
    SetSender addr -> do
      modify $ \s -> s { sender = addr }
      pure Nothing
    SetTimestamp dt -> pure $ Just
      (UpdateSimulationMsg key (Sim.ModifyTimestamp $ Sim.SetTimestamp dt))
    CallContract name args -> do
      s <- gets sender
      pure
        $ Just ( UpdateSimulationMsg key (Sim.CallMethod $ Sim.CallMethod' s name args))
    Query query -> pure $ Just (QuerySimulationMsg key query)
    Help -> do
      liftIO $ putText "Available Commands:"
      mapM_ displayCmdHelp reservedNames
      pure Nothing
    Quit -> liftIO exitSuccess


hoistErr :: (StringConv e Text) => Either e a -> REPLM a
hoistErr (Right val) = return val
hoistErr (Left err) = do
  liftIO $ Utils.putRed (toS err)
  REPLM abort

-------------------------------------------------------------------------------
-- Help
-------------------------------------------------------------------------------

displayCmdHelp :: Text -> REPLM ()
displayCmdHelp cmd = do
    liftIO $ Utils.putGreen $ cmd <> " " <> helpCmdArgs cmd
    putText $ "    " <> helpCmdInfo cmd <> "\n"

helpCmdInfo :: Text -> Text
helpCmdInfo c =
  if | c == setSender -> "set sender"
     | c == setTimestamp -> "set timestamp"
     | c == methods -> "print methods available in the contract"
     | c == assets -> "print assets"
     | c == contract -> "print state of the contract"
     | c == help -> "display this message"
     | c == ledger -> "print state of the ledger"
     | otherwise ->  ""

helpCmdArgs :: Text -> Text
helpCmdArgs c =
  if | c == setSender -> "<address>"
     | c == setTimestamp -> "<YYYY-MM-DDThh:mm:ss.sTZD>"
     | otherwise ->  ""
