module Console.Parser (
  parseConsoleCmd,
) where

import Protolude hiding ((<|>), try, optional)

import Data.List (foldl1)
import qualified Data.Text as T
import qualified Data.String as String

import Text.Parsec
import Text.Parsec.Text
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang

import Address (Address, parseAddr)
import Asset (Ref(..), AssetType(..))
import Script.Parser (contents, mkParseErrInfo, ParseErrInfo, textLit)
import qualified Utils

import Console.Command
import Console.Config
import Console.Lexer

import System.FilePath.Posix

literal :: Parser Text
literal = lexeme $ T.pack <$> many1 (noneOf " \n")

quoted :: Parser Text
quoted = between
  (symbol "\"")
  (symbol "\"")
  literal

p2pConsoleCmd :: Parser ConsoleCmd
p2pConsoleCmd =
      (reserved discover   >> pure Discover)
  <|> (reserved listPeers  >> pure ListPeers)
  <|> (reserved reconnect  >> pure Reconnect)
  <|> (reserved ping       >> pure Ping)
  <|> (reserved pingPeer   >> PingPeer <$> literal)
  <|> (reserved addPeer    >> AddPeer <$> literal)

txConsoleCmd :: Parser ConsoleCmd
txConsoleCmd =
      (reserved listAccounts   >> pure ListAccounts)
  <|> (reserved listAssets     >> pure ListAssets)
  <|> (reserved listContracts  >> pure ListContracts)
  <|> (reserved createAccount  >> CreateAccount . toS <$> literal)
  <|> (reserved createAsset    >> parseCreateAsset)
  <|> (reserved createContract >> CreateContract . toS <$> literal)
  <|> (reserved transferAsset  >> parseTransferAsset)
  <|> (reserved circulateAsset >> parseCirculateAsset)
  <|> (reserved callContract   >> pure CallContract)
  <|> (reserved transaction    >> Transaction . toS <$> literal)

miscConsoleCmd :: Parser ConsoleCmd
miscConsoleCmd =
      (reserved setAccount >> SetAccount . fmap toS <$> optionMaybe literal)
  <|> (reserved help >> pure Help)
  <|> (reserved quit >> pure Quit)

command :: Parser ConsoleCmd
command =
      p2pConsoleCmd
  <|> txConsoleCmd
  <|> miscConsoleCmd

consoleParse :: Parser a -> String.String -> Either ParseErrInfo a
consoleParse p input =
    first (mkParseErrInfo packed) $
      parse (lexeme p) "<console>" packed
  where
    packed = T.pack input

parseConsoleCmd :: String.String -> Either ParseErrInfo ConsoleCmd
parseConsoleCmd = consoleParse command

parseTransferAsset :: Parser ConsoleCmd
parseTransferAsset = TransferAsset
  <$> addressParser
  <*> addressParser
  <*> integer

parseCreateAsset :: Parser ConsoleCmd
parseCreateAsset = CreateAsset
  <$> literal
  <*> integer
  <*> lexeme assetRefParser
  <*> lexeme assetTypeParser

parseCirculateAsset :: Parser ConsoleCmd
parseCirculateAsset = CirculateAsset
  <$> addressParser
  <*> fmap fromInteger integer

assetRefParser :: Parser (Maybe Asset.Ref)
assetRefParser = optionMaybe $
    (foldl1 (\acc p -> acc <|> p) parsers <?> descr)
  where
    refs    = [minBound.. maxBound] :: [Asset.Ref]
    refStrs = map (show :: Ref -> String.String) refs

    mkParser ref refStr = string refStr *> pure ref
    parsers = zipWith mkParser refs refStrs

    descr = "Asset types: "
      <> intercalate ", " refStrs
      <> " or Nothing"

assetTypeParser :: Parser Asset.AssetType
assetTypeParser =
        discreteParser
    <|> binaryParser
    <|> between (char '\'') (char '\'') fractionalParser
    <?> "Discrete, Binary, or 'Fractional <Int>'"
  where
    discreteParser   = string "Discrete" *> pure Discrete
    binaryParser     = string "Binary"   *> pure Binary
    fractionalParser = do
      string "Fractional"
      int <- fromInteger <$> integer
      when (int `notElem` [1..6]) $
        parserFail "Fractional asset denominations must be between 1 and 7"
      pure $ Fractional int

addressParser :: Parser Address
addressParser = do
  s <- literal
  case Address.parseAddr (encodeUtf8 s) of
    Nothing -> parserFail "Invalid Address"
    Just addr -> pure addr
