{-# OPTIONS_GHC -fno-warn-missing-signatures  #-}
module Console.Lexer (
    reserved
  , lexeme
  , identifier
  , integer
  , reservedNames
  , symbol
  , discover
  , listPeers
  , reconnect
  , ping
  , pingPeer
  , addPeer

  , listAccounts 
  , listAssets
  , listContracts 

  , createAccount
  , createAsset
  , createContract
  , transferAsset
  , callContract
  , transaction

  , setAccount
  , help
  , quit
) where
import Protolude hiding ((<|>))
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang
import qualified Data.String as String
import Text.Parsec
import Text.Parsec.Text
import qualified Data.Text as T



lexer :: Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser style

style :: Tok.GenLanguageDef T.Text () Identity
style = Lang.emptyDef
  { Tok.commentStart    = "/*"
  , Tok.commentEnd      = "*/"
  , Tok.commentLine     = "//"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.opStart         = Tok.opLetter style
  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedOpNames = mempty
  , Tok.reservedNames   = map T.unpack reservedNames
  , Tok.caseSensitive   = True
  }


lexeme :: Parser a -> Parser a
lexeme = Tok.lexeme lexer

reserved :: T.Text -> Parser ()
reserved nm = Tok.reserved lexer (T.unpack nm)

integer :: Parser Integer
integer = Tok.integer lexer

identifier :: Parser String.String
identifier = Tok.identifier lexer

symbol :: T.Text -> Parser T.Text
symbol = fmap T.pack . Tok.symbol lexer . T.unpack


discover = "discover"
listPeers = "listPeers"
reconnect = "reconnect"
ping = "ping"
pingPeer = "pingPeer"
addPeer = "addPeer"
removePeer = "removePeer"

listAccounts = "listAccounts"
listAssets = "listAssets"
listContracts = "listContracts"

createAccount = "createAccount"
createAsset = "createAsset"
createContract = "createContract"
transferAsset = "transferAsset"
callContract = "callContract"
transaction = "transaction"

setAccount = "setAccount"
help = "help"
quit = "quit"

reservedNames :: [T.Text]
reservedNames = [
    discover
  , listPeers
  , reconnect
  , ping
  , pingPeer
  , addPeer
  
  , listAccounts 
  , listAssets
  , listContracts 

  , createAccount
  , createAsset
  , createContract
  , transferAsset
  , callContract
  , transaction
  , help
  , setAccount
  , quit
  ]
