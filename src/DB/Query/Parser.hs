{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}

module DB.Query.Parser where

import Protolude hiding (takeWhile)

import Control.Monad (fail)

import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.Text (singleton)
import Data.Attoparsec.Text

import qualified Data.List.NonEmpty as NE

import qualified Address
import qualified Asset
import qualified Encoding
import qualified Key
import qualified Storage
import qualified Script.Graph as Graph
import qualified Script.Token as Token

import Database.PostgreSQL.Simple.ToField
import DB.Query.Lang

whitespace, whitespaces :: Parser Text
whitespace  = takeWhile isSpace
whitespaces = takeWhile1 isSpace

lexeme :: Parser a -> Parser a
lexeme p = whitespaces *> p

queryTok, whereTok, andTok, orTok, semiTok :: Parser Text
queryTok = asciiCI "query"
whereTok = lexeme $ asciiCI "WHERE"
andTok   = lexeme $ asciiCI "AND"
orTok    = lexeme $ asciiCI "OR"
semiTok  = whitespace *> string ";"

--------------------------------------------------------------------------------

parseQuery :: Text -> Either Text Select
parseQuery = first toS . parseOnly (parseSelect <* endOfInput)

-- | The only "tables" that should be parsed are the tables that refer to
-- canonical ledger types: accounts, assets, contracts, transactions, and blocks.
parseSelect :: Parser Select
parseSelect = do
    queryTok
    table <- lexeme parseTable
    q <- parseSelect' table
    semiTok
    return q
  where
    parseSelect' :: Table -> Parser Select
    parseSelect' table = do
      case table of

        TableAccounts accTable ->
          parseSelect'' parseAccountCol $
            SelectAccounts . Select' accTable

        TableAssets assetsTable ->
          parseSelect'' parseAssetCol $
            SelectAssets . Select' assetsTable

        TableContracts contractsTable ->
          parseSelect'' parseContractCol $
            SelectContracts . Select' contractsTable

        TableBlocks blocksTable ->
          parseSelect'' parseBlockCol $
            SelectBlocks . Select' blocksTable

        TableTransactions txsTable ->
          parseSelect'' parseTransactionCol $
            SelectTransactions . Select' txsTable

        _ -> panic $ "Parser for table " <> show table <> " not implemented yet."

    parseSelect''
      :: (ToField a, HasColName a)
      => ColParser a
      -> (Maybe (WhereClause a) -> Select)
      -> Parser Select
    parseSelect'' parseCol mkSelect = do
      mWhereClause <- parserToMaybe $ parseWhereClause parseCol
      pure $ mkSelect mWhereClause

--------------------------------------------------------------------------------
-- Table Parser
--------------------------------------------------------------------------------

parseTable :: Parser Table
parseTable = do
  tableName' <- toS <$> takeWhile1 isAlphaNum
  pure $ if
    | tableName' == tableName AccountsTable      ->
        TableAccounts AccountsTable
    | tableName' == tableName AssetsTable        ->
        TableAssets AssetsTable
    | tableName' == tableName HoldingsTable      ->
        TableHoldings HoldingsTable
    | tableName' == tableName ContractsTable     ->
        TableContracts ContractsTable
    | tableName' == tableName GlobalStorageTable ->
        TableGlobalStorage GlobalStorageTable
    | tableName' == tableName LocalStorageTable  ->
        TableLocalStorage LocalStorageTable
    | tableName' == tableName BlocksTable        ->
        TableBlocks BlocksTable
    | tableName' == tableName TransactionsTable  ->
        TableTransactions TransactionsTable
    | otherwise  -> panic "Unrecognized table name"

--------------------------------------------------------------------------------
-- Where Clause & Condition Parsers
--------------------------------------------------------------------------------

type ColParser a = (Text -> Parser a)

parseWhereClause
  :: forall a. (HasColName a, ToField a)
  => ColParser a
  -> Parser (WhereClause a)
parseWhereClause parseCol =
    whereTok *> parseWheres
  where
    parseWheres =
      parseWhereConj <|> parseWhere

    parseWhere     =
      Where <$> parseCond parseCol
    parseWhereConj =
      WhereConj
        <$> parseCond parseCol
        <*> parseConj
        <*> parseWheres

    parseConj = parseAnd <|> parseOr
    parseAnd  = const And <$> andTok
    parseOr   = const Or  <$> orTok


-- | Given a column parser, parse a Where condition
-- Note: This function must take a "column parser" as an argument because the
-- type of the value parser on the RHS of the operation is specified by the name
-- of the column with which it will be compared to the value of on the LHS.
parseCond
  :: (HasColName a, ToField a)
  => (Text -> Parser a)      -- ^ Column Parser
  -> Parser (Cond a)
parseCond parseCol = do
    colName    <- whitespaces *> parseSQLIdentifier
    let parseColVal = whitespace *> parseCol colName
    whitespace *> parseColOp parseColVal
  where
    parseColOp parseColVal = choice
      [ parseColEq  <*> parseColVal
      , parseColGTE <*> parseColVal
      , parseColGT  <*> parseColVal
      , parseColLTE <*> parseColVal
      , parseColLT  <*> parseColVal
      , parseColIn  <*> parseIn parseColVal
      ]

    parseColEq  = char   '='  *> pure ColEq
    parseColGT  = char   '>'  *> pure ColGT
    parseColGTE = string ">=" *> pure ColGTE
    parseColLT  = char   '<'  *> pure ColLT
    parseColLTE = string "<=" *> pure ColLTE
    parseColIn  = string "in" *> pure ColIn

    parseIn parseColVal = do
      whitespace *> char '('
      vals <- parseColVal `sepBy1` (whitespace *> char ',')
      whitespace *> char ')'
      pure $ NE.fromList vals

--------------------------------------------------------------------------------
-- Column Parsers
--------------------------------------------------------------------------------

parseAccountCol :: Text -> Parser AccountCol
parseAccountCol "address"   = AccountAddress  <$> parseAddress
parseAccountCol "timezone"  = AccountTimezone <$> parseQuoted
parseAccountCol colName     = fail $
  "Could not parse AccountCol. Unrecognized col name: " <> show colName

--------------------------------------------------------------------------------

parseAssetCol :: Text -> Parser AssetCol
parseAssetCol "name"          = AssetName      <$> parseQuoted
parseAssetCol "issuer"        = AssetIssuer    <$> parseAddress
parseAssetCol "issuedOn"      = AssetIssuedOn  <$> decimal
parseAssetCol "issuedon"      = AssetIssuedOn  <$> decimal
parseAssetCol "supply"        = AssetSupply    <$> decimal
parseAssetCol "reference"     = AssetReference <$> parseReference
parseAssetCol "assetType"     = AssetType      <$> parseAssetType
parseAssetCol "assettype"     = AssetType      <$> parseAssetType
parseAssetCol "address"       = AssetAddress   <$> parseAddress
parseAssetCol "holder"        = HoldingsCol    <$> parseHoldingsCol "holder"
parseAssetCol colName         = fail $
  "Could not parse AssetCol. Unrecognized col name: " <> show colName

parseReference :: Parser Asset.Ref
parseReference = do
  mRef :: Maybe Asset.Ref <-
    readMaybe . toS <$> parseQuoted
  case mRef of
    Nothing  -> fail "Failed to parse Ref"
    Just ref -> pure ref

parseAssetType :: Parser Asset.AssetType
parseAssetType = do
  mAType :: Maybe Asset.AssetType <-
    readMaybe . toS <$> parseQuoted
  case mAType of
    Nothing    -> fail "Failed to parse AssetType"
    Just atype -> pure atype

--------------------------------------------------------------------------------

parseHoldingsCol :: Text -> Parser HoldingsCol
parseHoldingsCol "asset"   = HoldingsAsset   <$> parseAddress
parseHoldingsCol "holder"  = do
  a <- parseAddress
  pure $ HoldingsHolder (Asset.Holder (a :: Address.Address Address.AAccount))

parseHoldingsCol "balance" = HoldingsBalance <$> decimal
parseHoldingsCol colName   = fail $
  "Could not parse HoldingsCol. Unrecognized col name: " <> show colName

--------------------------------------------------------------------------------

parseContractCol :: Text -> Parser ContractCol
parseContractCol "timestamp" = ContractTimestamp <$> decimal
parseContractCol "state"     = ContractState     <$> parseGraphState
parseContractCol "owner"     = ContractOwner     <$> parseAddress
parseContractCol "address"   = ContractAddress   <$> parseAddress
parseContractCol colName     = fail $
  "Could not parse ContractsCol. Unrecognized col name: " <> show colName

parseGraphState :: Parser Graph.GraphState
parseGraphState = do
  label <- parseQuoted
  pure $ if
    | label == Token.initial -> Graph.GraphInitial
    | label == Token.terminal -> Graph.GraphTerminal
    | otherwise -> Graph.GraphLabel (Graph.Label label)

--------------------------------------------------------------------------------

parseGlobalStorageCol :: Text -> Parser GlobalStorageCol
parseGlobalStorageCol "contract" = GSContract <$> parseAddress
parseGlobalStorageCol "key"      = GSKey      <$> parseStorageKey
parseGlobalStorageCol colName    = fail $
  "Could not parse GlobalStorageCol. Unrecognized col name: " <> show colName

parseLocalStorageCol :: Text -> Parser LocalStorageCol
parseLocalStorageCol "contract" = LSContract <$> parseAddress
parseLocalStorageCol "account"  = LSAccount  <$> parseAddress
parseLocalStorageCol "key"      = LSKey      <$> parseStorageKey
parseLocalStorageCol colName    = fail $
  "Could not parse LocalStorageCol. Unrecognized col name: " <> show colName

parseStorageKey :: Parser Storage.Key
parseStorageKey = Storage.Key . toS <$>
  (parseDoubleQuoted <|> parseSingleQuoted)

--------------------------------------------------------------------------------

parseBlockCol :: Text -> Parser BlockCol
parseBlockCol "idx"       = BlockIdx       <$> decimal
parseBlockCol "origin"    = BlockOrigin    <$> parseAddress
parseBlockCol "timestamp" = BlockTimestamp <$> decimal
parseBlockCol colName     = fail $
  "Could not parse BlockCol. Unrecognized col name: " <> show colName

--------------------------------------------------------------------------------

parseTransactionCol :: Text -> Parser TransactionCol
parseTransactionCol "origin"    = TxOrigin    <$> parseAddress
parseTransactionCol "tx_type"   = TxType      <$> parseQuoted
parseTransactionCol "hash"      = TxHash <$> parseQuoted
parseTransactionCol colName     = fail $
  "Could not parse TransactionCol. Unrecognized col name: " <> show colName

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

parserToMaybe :: Parser a -> Parser (Maybe a)
parserToMaybe parser = fmap Just parser <|> pure Nothing

-- | Parses a SQL identifier for table names, column names, etc
-- Note: not comprehensive, but good enough for our purposes
parseSQLIdentifier :: Parser Text
parseSQLIdentifier =
  (<>) <$> (singleton <$> satisfy (\c -> c == '_' || isAlpha c))
       <*> takeWhile (\c -> isAlphaNum c || c `elem` ("_$" :: [Char]))

parseDoubleQuoted :: Parser Text
parseDoubleQuoted = do
  char '\"' *> (takeWhile $ not . (==) '\"') <* char '\"'

parseSingleQuoted :: Parser Text
parseSingleQuoted = do
  char '\'' *> (takeWhile $ not . (==) '\'') <* char '\''

parseQuoted :: Parser Text
parseQuoted = parseSingleQuoted <|> parseDoubleQuoted

parseAddress :: Parser (Address.Address a)
parseAddress = do
  _ <- char '\''
  addr <- takeWhile1 isAlphaNum
  _ <- char '\''
  case Address.parseAddress (encodeUtf8 addr) of
    Left err -> fail "Could not parse Address"
    Right addr -> pure addr

parsePubKey :: Parser Key.PubKey
parsePubKey = do
  pk <- takeWhile1 isHexDigit
  case Encoding.parseEncodedBS (encodeUtf8 pk) of
    Left (Encoding.BadEncoding err) -> fail $ "Could not parse PubKey: " <> (show err)
    Right b -> case Key.decodeHexPub (Key.HexPub b) of
      Left err -> fail $ "Could not parse PubKey" <> err
      Right pubKey -> pure pubKey
  where
    isHexDigit c =
      (c >= '0' && c <= '9') ||
      (c >= 'a' && c <= 'f') ||
      (c >= 'A' && c <= 'F')
