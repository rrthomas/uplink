{-# LANGUAGE MultiWayIf #-}
module Console.Command (
  ConsoleCmd(..),

  getCmd,
  handleConsoleCmd,

  accountPrompt,
  displayCmdHelp,
  displayActiveAccount

) where

import Protolude hiding (newChan)

import Control.Distributed.Process hiding (reconnect)

import System.Console.Repline

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.String as String
import qualified Data.Serialize as S

import Account (Account)
import Asset (Holder(..))
import Address (Address, AAsset)
import qualified Time
import qualified Transaction
import qualified Key
import qualified Address
import qualified Asset
import qualified Metadata
import qualified Account
import qualified Network.P2P.Cmd as Cmd
import qualified Utils
import qualified SafeString

import Script.Parser (commaSep, lit, parens)
import Script (evalLit)

import Console.Config
import Console.Lexer

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data ConsoleCmd =
  -- Network
    Discover
  | ListPeers
  | Reconnect
  | Ping
  | PingPeer Text
  | AddPeer Text

  -- Inspect ledger state
  | ListAccounts
  | ListAssets
  | ListContracts

  -- Transactions
  | CreateAccount FilePath
  | CreateAsset Text Integer (Maybe Asset.Ref) Asset.AssetType
  | CreateContract FilePath
  | TransferAsset (Address AAsset) Holder Integer
  | CirculateAsset (Address AAsset) Integer
  | CallContract

  | Transaction FilePath
  | TransactionRaw Transaction.TransactionHeader

  -- Misc
  | SetAccount (Maybe FilePath)
  | Help
  | Quit
  deriving (Show)

-------------------------------------------------------------------------------
-- Command handling
-------------------------------------------------------------------------------

handleConsoleCmd :: ProcessId -> Cmd.Cmd -> Process ()
handleConsoleCmd cmdProc cmd = do
  -- Send cmd to remote node
  (sp,rp) <- newChan
  send cmdProc (cmd, sp)
  -- Receive result from remote node
  res <- receiveChan rp
  case res of
    (Cmd.CmdResult t) -> liftIO $ Utils.putGreen t
    (Cmd.Accounts accs) -> putText $ toS $ encodePretty accs
    (Cmd.Assets assets) -> putText $ toS $ encodePretty assets
    (Cmd.Contracts contracts) -> putText $ toS $ encodePretty contracts
    (Cmd.PeerList peers) -> putText $ show peers
    (Cmd.CmdFail t) -> liftIO $ Utils.putRed t

getCmd :: ConsoleCmd -> ConsoleM (Maybe Cmd.Cmd)
getCmd c = do
  consoleState <- get

  case c of
    Discover -> return $ Just Cmd.Discover
    ListPeers -> return $ Just Cmd.ListPeers
    Reconnect -> return $ Just Cmd.Reconnect
    Ping -> return $ Just Cmd.Ping
    (PingPeer host) -> return $ Just (Cmd.PingPeer host)
    (AddPeer host) -> return $ Just (Cmd.AddPeer host)

    ListAccounts -> return $ Just Cmd.ListAccounts
    ListAssets-> return $ Just Cmd.ListAssets
    ListContracts -> return $ Just Cmd.ListContracts
    CreateAccount path -> do
      file <- liftIO $ Utils.safeRead path
      contents <- hoistErr file
      let privE = Key.importPriv contents
      case privE of
        Left err -> do
          print err
          return Nothing
        Right priv -> do
          name <- liftIO $ Utils.prompt "Name: "
          tz <- liftIO $ Utils.prompt "Timezone: "

          let metadata' = Metadata.Metadata $ Map.singleton "name" name
              hdr = Transaction.TxAccount $
                Transaction.CreateAccount
                  { pubKey   = SafeString.fromBytes' $ Key.exportPub $ fst priv
                  , timezone = SafeString.fromBytes' $ encodeUtf8 tz
                  , metadata = metadata'
                  }

          getCmd (TransactionRaw hdr)

    CreateAsset name supply mref typ -> do
      mAcc <- gets account
      acc <- case mAcc of
        Nothing -> accountPrompt Nothing
        Just acc -> pure acc
      ts <- liftIO Time.now
      let hdr = Transaction.TxAsset $
            Transaction.CreateAsset {
                assetName = SafeString.fromBytes' $ toS name
              , supply    = fromInteger supply
              , reference = mref
              , assetType = Asset.Discrete
              , metadata = mempty -- XXX make metadata prompt
              }
      getCmd (TransactionRaw hdr)

    (CreateContract path) -> do
      timestamp <- liftIO Time.now
      file <- liftIO $ Utils.safeRead path
      contents <- hoistErr file

      let hdr = Transaction.TxContract
            Transaction.CreateContract
              { contract = SafeString.fromBytes' $ toS contents }
      getCmd (TransactionRaw hdr)

    TransferAsset assetAddr' toAddr amount -> do
      let hdr = Transaction.TxAsset Transaction.Transfer {
            assetAddr = assetAddr'
          , toAddr    = toAddr
          , balance   = fromInteger amount
          }
      getCmd (TransactionRaw hdr)

    CirculateAsset addr amount -> do
      let hdr = Transaction.TxAsset Transaction.Circulate {
            assetAddr = addr
          , amount    = fromInteger amount
          }
      getCmd (TransactionRaw hdr)

    CallContract -> do
      contract <- liftIO $ Utils.prompt "Contract address: "
      contractAddr <- consoleParseAddr contract
      method <- liftIO $ Utils.prompt "Method name:  "
      args <- Utils.parsePrompt "Args (v1,v2...): " $ parens $ commaSep lit
      let hdr = Transaction.TxContract Transaction.Call {
        address = contractAddr
      , method = encodeUtf8 method
      , args = map evalLit args
      }
      getCmd (TransactionRaw hdr)

    Transaction path -> do
      contents <- liftIO $ BS.readFile path

      let hdrE = eitherDecodeStrict contents :: Either String.String Transaction.TransactionHeader
      hdr <- hoistErr $ first toS $ hdrE
      getCmd (TransactionRaw hdr)

    (TransactionRaw hdr) -> do
        txE <- createTx hdr
        case txE of
          Left err -> do
            print err
            return Nothing
          Right tx -> do
            previewTx tx
            continue <- liftIO $ Utils.yesOrNoPrompt "Continue? (y/n) : "
            if continue
              then do
                putText "Transaction submitted"
                return $ Just (Cmd.Transaction tx)
              else do
                putText "Cancelled submitting transaction"
                return Nothing

    SetAccount fp -> do
      accountPrompt fp
      return Nothing

    Help -> do
      putText "Available Commands:"
      forM_ reservedNames displayCmdHelp
      return Nothing

    Quit -> liftIO exitSuccess

-------------------------------------------------------------------------------
-- Transaction Utils
-------------------------------------------------------------------------------

createTx
  :: Transaction.TransactionHeader
  -> ConsoleM (Either Text Transaction.Transaction)
createTx hdr = do
  consoleState <- get
  case (privKey consoleState, account consoleState) of
    (Just privKey, Just acc) -> do
      sig <- liftIO $ Key.sign privKey $ S.encode hdr
      let issuer = Account.address acc
      return $ Right Transaction.Transaction {
          header    = hdr
        , origin    = Account.address acc
        , signature = Key.encodeSig sig
        }
    _ -> do
      putText "Attempting to issue a transaction, but no account is set."
      accountPrompt Nothing
      createTx hdr


previewTx :: Transaction.Transaction -> ConsoleM ()
previewTx tx = putText $ toS $ encodePretty tx

hoistErr :: Either Text a -> ConsoleM a
hoistErr (Right val) = return val
hoistErr (Left err) = do
  liftIO $ Utils.putRed err
  ConsoleM abort

accountPrompt :: Maybe FilePath -> ConsoleM Account
accountPrompt (Just path) = do
  eAccAndKeys <- liftIO $ Account.readAccountData $ toS path
  (acc, (_,privKey)) <- hoistErr eAccAndKeys
  modify $ \consoleState ->
    consoleState { account = Just acc, privKey = Just privKey }
  displayActiveAccount
  pure acc
accountPrompt Nothing = do
  useExistingAccount <- liftIO $
    Utils.yesOrNoPrompt "Would you like to use an existing account (y/n) ? "
  if useExistingAccount
    then do
      path <- liftIO $ Utils.prompt "Please provide the directory containing the account you wish to use: "
      accountPrompt (Just $ toS path)
    else do
      path <- liftIO $ toS <$>
        Utils.prompt "Please provide the directory to store the new account in: "

      dir <- liftIO $ Account.createAccountDir path
      hoistErr dir

      acc <- liftIO $ Account.createAccPrompt path Nothing Account.Prompt
      accountPrompt (Just path)

-------------------------------------------------------------------------------
-- Parse args on Cmd line
-------------------------------------------------------------------------------

consoleParseAddr :: Text -> ConsoleM (Address a)
consoleParseAddr addr = hoistErr $
  case Address.parseAddress (encodeUtf8 addr)  of
    Left err -> Left "Address given is invalid"
    Right addr -> Right addr

-------------------------------------------------------------------------------
-- Help
-------------------------------------------------------------------------------

consoleCmdHelp :: Text -> Text
consoleCmdHelp c =
  if | c == discover -> "force discovery of new peers"
     | c == listPeers -> "list peers"
     | c == reconnect -> "reconnect"
     | c == ping -> "ping all peers"
     | c == pingPeer -> "ping a specific peer eg. pingPeer 127.0.1.1"
     | c == addPeer -> "discover a peer eg. addPeer 143.245.35.1"
     | c == listAccounts -> "print all acccounts"
     | c == listAssets -> "print all assets"
     | c == listContracts -> "print all contracts"
     | c == createAccount -> "create account eg. createAccount key.priv"
     | c == createAsset -> "create an asset eg. createAsset USD 1000 USD Discrete"
     | c == createContract -> "create contract eg. createContract contracts/escrow.s"
     | c == transferAsset -> "transfer asset holdings"
     | c == circulateAsset -> "circulate asset holdings"
     | c == callContract -> "call contract"
     | c == transaction -> "submit a transaction eg. transaction tests/testTxHeader.json"
     | c == help -> "display this message"
     | c == setAccount -> "set active account transactions are submitted from eg setAccount or setAccount alice"
     | c == quit -> "exit the console"
     | otherwise ->  ""

consoleCmdHelpUsage :: Text -> Text
consoleCmdHelpUsage c =
  if | c == pingPeer -> " <node>"
     | c == addPeer -> " <node>"
     | c == createAccount -> " <private key path>"
     | c == createContract -> " <fcl source path>"
     | c == transaction -> " <path of json transaction header>"
     | c == createAsset -> " <name> <supply> <ref (optional)> <type>"
     | c == transferAsset -> " <asset address> <recipient address> <amount>"
     | c == circulateAsset -> " <address> <amount>"
     | c == setAccount -> " <optional path to account dir>"
     | otherwise ->  ""

-------------------------------------------------------------------------------
-- Display Utils
-------------------------------------------------------------------------------

displayCmdHelp :: Text -> ConsoleM ()
displayCmdHelp cmd = do
  when (cmd `elem` reservedNames) $ do
    liftIO $ Utils.putGreen $ cmd <> consoleCmdHelpUsage cmd <>  ":"
    putText $ "    " <> consoleCmdHelp cmd <> "\n"

displayActiveAccount :: ConsoleM ()
displayActiveAccount = do
  consoleState <- get
  case (privKey consoleState, account consoleState) of
    (Just privKey, Just acc) -> do
      let address = Address.rawAddr $ Account.address acc
      liftIO $ Utils.putGreen $ "Active account: " <> decodeUtf8 address
    _ -> putText "No account is active."
