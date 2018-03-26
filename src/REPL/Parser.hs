module REPL.Parser (
  parseREPLCmd,
) where
import Protolude hiding ((<|>), try, optional)

import Text.Parsec
import Text.Parsec.Text
import Console.Parser
import Script (unDateTime, Expr(ECall), evalLit)
import Script.Parser

import Console.Lexer (symbol, reserved, lexeme)
import REPL.Lexer
import REPL.Command

import Network.P2P.Simulate hiding (SetTimestamp)
import Data.Text as T

replCmd :: Parser REPLCmd
replCmd =
        (reserved setSender >> SetSender <$> addressParser)
    <|> (reserved setTimestamp >> SetTimestamp <$> fmap unDateTime datetimeParser)
    <|> (reserved help >> pure Help)
    <|> (reserved methods >> pure (Query $ QueryMethods QueryMethods'))
    <|> (reserved assets >> pure (Query $ QueryAssets QueryAssets'))
    <|> (reserved contract >> pure (Query $ QueryContract QueryContract'))
    <|> (reserved ledger >> pure (Query $ QueryLedgerState QueryLedgerState'))
    <|> do
          methodName <- name
          args       <- parens $ commaSep lit
          return $ CallContract methodName (fmap evalLit args)

parseREPLCmd :: [Char] -> Either ParseErrInfo REPLCmd
parseREPLCmd input =
  first (mkParseErrInfo packed) $
    parse (lexeme replCmd) "<repl>" packed
  where
    packed = T.pack input
