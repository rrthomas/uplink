module Console.Parser (
  parseConsoleCmd,
) where
import Protolude hiding ((<|>), try, optional)
import  System.FilePath.Posix
import Text.Parsec
import Text.Parsec.Text
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang
import qualified Data.String as String
import Script.Parser (contents, mkParseErrInfo, ParseErrInfo, textLit)
import qualified Utils
import Console.Command
import Console.Config

import Console.Lexer

import qualified Data.Text as T

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
  <|> (reserved createAsset    >> CreateAsset . toS <$> quoted <*> integer)
  <|> (reserved createContract >> CreateContract . toS <$> literal)
  <|> (reserved transferAsset  >> pure TransferAsset)
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

parseConsoleCmd :: String.String -> Either ParseErrInfo ConsoleCmd
parseConsoleCmd input = first (mkParseErrInfo packed) 
  $ parse (lexeme command) "<stdin>" packed
  where
    packed = T.pack input

