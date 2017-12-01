{-# LANGUAGE MultiWayIf #-}
module Console.Command (
  ConsoleCmd(..),
  accountPrompt,
  handleConsoleCmd,
  displayCmdHelp,
  displayActiveAccount

) where

import Protolude

import Control.Monad.Base
import System.Console.Repline

import qualified Data.String as String
import qualified Data.Set as Set
import Data.Aeson
import Data.Aeson.Encode.Pretty

import Text.Parsec

import Account (Account)
import Address (Address)
import Derivation (addrAsset)
import qualified Time
import qualified NodeState
import qualified Transaction
import qualified Key
import qualified Derivation
import qualified Storage
import qualified Address
import qualified Asset
import qualified Account
import qualified Data.ByteString as BS
import qualified Network.P2P.Cmd as Cmd
import qualified Data.Map as Map
import qualified Validate
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
  | CreateAsset Text Integer (Maybe Asset.Ref) (Asset.AssetType)
  | CreateContract FilePath
  | TransferAsset Address Address Integer
  | CirculateAsset Address Integer
  | CallContract

  | Transaction FilePath
  | TransactionRaw Transaction.TransactionHeader (Maybe Time.Timestamp)

  -- Misc
  | SetAccount (Maybe FilePath)
  | Help
  | Quit
  deriving (Show)

-------------------------------------------------------------------------------
-- Command handling
-------------------------------------------------------------------------------

handleConsoleCmd :: ConsoleCmd -> Console ()
handleConsoleCmd consoleCmd = do
  chan <- lift $ asks chan
  resultChan <- lift $ asks resultChan

  rpcCmd <- getCmd consoleCmd
  case rpcCmd of
    (Just cmd) -> do
      -- Send cmd to remote node
      liftIO $ writeChan chan cmd
      -- Receive result from remote node
      res <- liftIO $ readChan resultChan
      case res of
        (Cmd.CmdResult t) -> liftIO $ Utils.putGreen t
        (Cmd.Accounts accs) -> putText $ toS $ encodePretty accs
        (Cmd.Assets assets) -> putText $ toS $ encodePretty assets
        (Cmd.Contracts contracts) -> putText $ toS $ encodePretty contracts
        (Cmd.PeerList peers) -> putText $ show peers
    Nothing -> return ()

getCmd :: ConsoleCmd -> Console (Maybe Cmd.Cmd)
getCmd c = do
  ctx <- get

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

          let hdr = Transaction.TxAccount Transaction.CreateAccount {
            pubKey = Key.exportPub $ fst priv
          , timezone = encodeUtf8 tz
          , metadata = Account.Metadata $
                         Map.singleton "name" $ encodeUtf8 name
          }

          getCmd (TransactionRaw hdr Nothing)

    CreateAsset name supply mref typ -> do
      mAcc <- gets account
      acc <- case mAcc of
        Nothing -> accountPrompt Nothing
        Just acc -> pure acc
      hdrTs <- liftIO Time.now
      let addr = addrAsset (toS name) (Account.address acc) (fromInteger supply) mref typ hdrTs
      let hdr = Transaction.TxAsset $
            Transaction.CreateAsset {
                assetAddr = addr
              , assetName = SafeString.fromBytes' $ toS name
              , supply    = fromInteger supply
              , reference = Just Asset.Token
              , assetType = Asset.Discrete
              }
      getCmd (TransactionRaw hdr $ Just hdrTs)

    (CreateContract path) -> do
      timestamp <- liftIO Time.now
      file <- liftIO $ Utils.safeRead path
      contents <- hoistErr file

      let addr = Derivation.addrContract' timestamp (Storage.GlobalStorage Map.empty)
      let hdr = Transaction.TxContract Transaction.CreateContract {
        address = addr
      , contract = SafeString.fromBytes' $ toS contents
      }
      getCmd (TransactionRaw hdr Nothing)

    TransferAsset assetAddr' toAddr amount -> do
      let hdr = Transaction.TxAsset Transaction.Transfer {
            assetAddr = assetAddr'
          , toAddr    = toAddr
          , balance   = fromInteger amount
          }
      getCmd (TransactionRaw hdr Nothing)

    CirculateAsset addr amount -> do
      let hdr = Transaction.TxAsset Transaction.Circulate {
            assetAddr = addr
          , amount    = fromInteger amount
          }
      getCmd (TransactionRaw hdr Nothing)

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
      getCmd (TransactionRaw hdr Nothing)

    Transaction path -> do
      contents <- liftIO $ BS.readFile path

      let hdrE = eitherDecodeStrict contents :: Either String.String Transaction.TransactionHeader
      hdr <- hoistErr $ first toS $ hdrE
      getCmd (TransactionRaw hdr Nothing)

    (TransactionRaw hdr mTs) -> do
        txE <- maybe (createTx hdr) (createTx' hdr) mTs
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
  -> Console (Either Text Transaction.Transaction)
createTx hdr = createTx' hdr =<< liftIO Time.now

createTx'
  :: Transaction.TransactionHeader
  -> Time.Timestamp
  -> Console (Either Text Transaction.Transaction)
createTx' hdr timestamp = do
  ctx <- get
  case (privKey ctx, account ctx) of
    (Just privKey, Just acc) -> do
      sig <- liftIO $ Key.signS privKey hdr
      let issuer = Account.address acc
      return $ Right Transaction.Transaction {
          header    = hdr
        , origin    = Account.address acc
        , signature = Key.encodeSig sig
        , timestamp = timestamp
        }
    _ -> do
      putText "Attempting to issue a transaction, but no account is set."
      accountPrompt Nothing
      createTx hdr


previewTx :: Transaction.Transaction -> Console ()
previewTx tx = putText $ toS $ encodePretty tx

hoistErr :: Either Text a -> Console a
hoistErr (Right val) = return val
hoistErr (Left err) = do
  liftIO $ Utils.putRed err
  abort

accountPrompt :: Maybe FilePath -> Console Account
accountPrompt (Just path) = do
  eAccAndKeys <- liftIO $ Account.loadAccount $ toS path
  (acc, (_,privKey)) <- hoistErr eAccAndKeys
  modify (\ctx -> ctx { account = Just acc, privKey = Just privKey })
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

      acc <- liftIO $ Account.setupAccount path Nothing Account.Prompt
      accountPrompt (Just path)

-------------------------------------------------------------------------------
-- Parse args on Cmd line
-------------------------------------------------------------------------------

consoleParseAddr :: Text -> Console Address.Address
consoleParseAddr addr = hoistErr $
  case (Address.parseAddr $ encodeUtf8 addr) of
    Nothing   -> Left "Address given is invalid"
    Just addr -> Right addr

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
     | c == createAsset -> "create an asset eg. createAsset \"USD\" 1000 USD Discrete"
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
     | c == setAccount -> " <optional path to account dir to use"
     | otherwise ->  ""

-------------------------------------------------------------------------------
-- Display Utils
-------------------------------------------------------------------------------

displayCmdHelp :: Text -> Console ()
displayCmdHelp cmd = do
  when (cmd `elem` reservedNames) $ do
    liftIO $ Utils.putGreen $ cmd <> consoleCmdHelpUsage cmd <>  ":"
    putText $ "    " <> consoleCmdHelp cmd <> "\n"

displayActiveAccount :: Console ()
displayActiveAccount = do
  ctx <- get
  case (privKey ctx, account ctx) of
    (Just privKey, Just acc) -> do
      let address = Address.rawAddr $ Account.address acc
      liftIO $ Utils.putGreen $ "Active account: " <> decodeUtf8 address
    _ -> putText "No account is active."
