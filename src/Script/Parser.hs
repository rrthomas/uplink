{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|

Parser for the FCL scripting language.

-}

module Script.Parser (
  -- ** Parser
  parseExpr,
  parseScript,
  parseText,
  parseFile,

  parseDefn,
  parseLit,
  parseType,
  parseFixedN,
  parseTimeDelta,
  parseDateTime,

  expr,
  callExpr,
  datetimeParser,

  -- ** Parser Errors
  ParseError,
  ParseErrInfo(..),
  mkParseErrInfo,
  errorMessages,
  messageString,

  fixedLit,
  textLit,
  contents,
  arg,
  lit,
  commaSep,
  parens,
  name,

  testParse,
  ) where

import Protolude hiding
  ((<|>), (<>), bool, many, try, option, optional, sourceLine, sourceColumn, Type)

import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Expr as Expr
import Text.ParserCombinators.Parsec.Error
import qualified Text.Parsec.Token as Tok

import Data.Char (digitToInt)
import Data.Functor.Identity (Identity)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Aeson (ToJSON(..), FromJSON)

import Fixed
import Script hiding (mapType)
import Address
import Script.Lexer as Lexer
import Script.Pretty hiding (parens)
import Script.Prim (lookupPrim)
import qualified SafeString as SS
import qualified Script.Token as Token
import qualified Datetime.Types as DT

-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------

-- | Parse an expression.
parseExpr :: T.Text -> Either ParseErrInfo LExpr
parseExpr input = first (mkParseErrInfo input)
  $ parse (contents expr) "<stdin>" input

-- | Parse file contents into a script.
parseScript :: T.Text -> Either ParseErrInfo Script
parseScript input = first (mkParseErrInfo input)
  $ parse (contents script <* eof) "<stdin>" input

-- | Parse text not expecting eof
parseText :: Text -> Either ParseErrInfo Script
parseText input = first (mkParseErrInfo input)
  $ parse (contents script) mempty input

-- | Parse a file into a script.
parseFile :: MonadIO m => FilePath -> m (Either ParseErrInfo Script)
parseFile filename = do
  program <- liftIO $ TIO.readFile filename
  return $ first (mkParseErrInfo program)
    $ parse (contents script <* eof) filename program

parseDefn :: Text -> Either ParseErrInfo Def
parseDefn input = first (mkParseErrInfo input)
  $ parse (contents def) "definition" input

parseLit :: Text -> Either ParseErrInfo Lit
parseLit input = first (mkParseErrInfo input)
  $ parse (contents lit) "literal" input

parseType :: Text -> Either ParseErrInfo Type
parseType input = first (mkParseErrInfo input)
  $ parse (contents type_) "type" input

parseFixedN :: Text -> Either ParseErrInfo FixedN
parseFixedN input = first (mkParseErrInfo input)
  $ parse (contents fixedN) "fixedN" input

parseTimeDelta :: Text -> Either ParseErrInfo TimeDelta
parseTimeDelta input = first (mkParseErrInfo input)
  $ parse timedeltaParser "timedelta" input

parseDateTime :: Text -> Either ParseErrInfo DateTime
parseDateTime input = first (mkParseErrInfo input)
  $ parse datetimeParser "datetime" input

contents :: Parser a -> Parser a
contents p = whiteSpace *> p

testParse :: T.Text -> IO ()
testParse = parseTest (contents script)


-------------------------------------------------------------------------------
-- Lit
-------------------------------------------------------------------------------

lit :: Parser Lit
lit =  fixedLit
   <|> floatLit
   <|> timedeltaLit
   <|> int64Lit
   <|> boolLit
   <|> addressLit
   <|> stateLit
   <|> datetimeLit
   <|> msgLit
   <|> voidLit
   <|> enumConstrLit
   <|> mapLit
   <|> setLit
   <?> "literal"

locLit :: Parser LLit
locLit = mkLocated lit

maxi64 :: Integer
maxi64 = fromIntegral (maxBound :: Int64)

mini64 :: Integer
mini64 = fromIntegral (minBound :: Int64)

int64Lit :: Parser Lit
int64Lit = do
  n <- Tok.integer lexer
  if n > maxi64
    then parserFail "Integer overflows int64"
    else if n < mini64
      then parserFail "Integer underflows int64"
      else pure (LInt (fromIntegral n))

floatLit :: Parser Lit
floatLit = try $ do
  mNeg <- optionMaybe $ char '-'
  LFloat <$> case mNeg of
    Nothing -> Tok.float lexer
    Just neg -> negate <$> try (Tok.float lexer)
   <?> "float literal"

fixedNLit :: PrecN -> Parser FixedN
fixedNLit precn = do
  let prec = fromEnum precn + 1
  try $ do
    -- Parse the left hand side of the decimal point
    sign <- maybe 1 (const (-1)) <$> optionMaybe (char '-')
    lhs <- Tok.integer lexer <* char '.'

    -- Parse the rhs, prec # of digits ending in 'f'
    rhs' <- count prec digit <* lexeme (char 'f')
    let rhs = map (toInteger . digitToInt) rhs'

    -- Convert the rhs to an integer
    let decs = sum $ zipWith (\n e -> n*(10^e)) (reverse rhs) [0..]

    -- Turn the lhs and rhs into a fixed point
    pure $ mkFixed precn $ sign * (lhs*(10^prec) + decs)

fixedN :: Parser FixedN
fixedN =  fixedNLit Prec6
      <|> fixedNLit Prec5
      <|> fixedNLit Prec4
      <|> fixedNLit Prec3
      <|> fixedNLit Prec2
      <|> fixedNLit Prec1
      <?> "fixed point number with 1-6 decimal places, ending in an 'f'"

fixedLit :: Parser Lit
fixedLit = LFixed <$> fixedN

boolLit :: Parser Lit
boolLit = (fmap LBool
  $  False <$ try (reserved Token.false)
 <|> True  <$ try (reserved Token.true))
 <?> "boolean literal"

rawAddress :: Parser (Address a)
rawAddress
  = Address.fromRaw . BS8.pack <$> between (symbol "\'") (symbol "\'") (many1 alphaNum)

addressLit :: Parser Lit
addressLit = try $ do
  type_ <- char 'c' <|> char 'a' <|> char 'u'
  if | type_ == 'c' -> LContract <$> rawAddress
     | type_ == 'a' -> LAsset <$> rawAddress
     | type_ == 'u' -> LAccount <$> rawAddress
     | otherwise    -> parserFail "Cannot parser address literal"

datetimeParser :: Parser DateTime
datetimeParser = try $ do
  isoStr <- textLit
  case DT.parseDatetime isoStr of
    Nothing -> parserFail "Invalid ISO8601 datetime string"
    Just datetime -> case DT.validateDatetime datetime of
      Left err -> parserFail "Invalid datetime specified"
      Right _ -> pure $ DateTime datetime

datetimeLit :: Parser Lit
datetimeLit = LDateTime <$> datetimeParser

timedeltaParser :: Parser TimeDelta
timedeltaParser = do
    years  <- DT.years  <$> parseYear
    months <- DT.months <$> parseMonth
    days   <- DT.days   <$> parseDay
    hours  <- DT.hours  <$> parseHour
    mins   <- DT.mins   <$> parseMin
    secs   <- DT.secs   <$> parseSec
    let delta = mconcat [ years, months, days, hours, mins, secs ]
    if delta == DT.years 0
      then parserFail "TimeDelta must not be 0"
      else pure $ TimeDelta delta
  where
    parseNat suffix = fmap fromIntegral $
      option 0 (try $ Tok.natural lexer <* string suffix)

    parseYear  = parseNat "y"
    parseMonth = parseNat "mo"
    parseDay   = parseNat "d"

    parseHour  = parseNat "h"
    parseMin   = parseNat "m"
    parseSec   = parseNat "s"

timedeltaLit :: Parser Lit
timedeltaLit = LTimeDelta <$> timedeltaParser

msgLit :: Parser Lit
msgLit = LMsg . SS.fromBytes' . BS8.pack <$> textLit
 <?> "msg"

stateLit :: Parser Lit
stateLit = do
  char ':'
  label <- labelIdent
  pure (LState label)
 <?> "state literal"

textLit :: Parser [Char]
textLit = do
    txt <- between
      (symbol "\"")
      (symbol "\"")
      (many ascii)
    return txt
 <?> "text literal"
  where
    ascii = alphaNum
         <|> (oneOf $ "!@#$%^&*()-=_+[]{};:',<.>/?\\| ")

voidLit :: Parser Lit
voidLit = LVoid <$ try (reserved Token.void)
 <?> "void literal"

enumConstrLit :: Parser Lit
enumConstrLit = LConstr <$> try (symbol "`" *> Lexer.enumConstr)

mapLit :: Parser Lit
mapLit =
  LMap . Map.fromList <$>
    braces (commaSep parseMapItem <* optional newline)
  where
    parseMapItem = do
      klit <- lit
      lexeme (symbol ":")
      vlit <- lit
      optional newline
      pure (klit, vlit)

setLit :: Parser Lit
setLit =
  LSet . Set.fromList <$>
    try (parens (commaSep lit <* optional newline))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type_ :: Parser Type
type_ =  intType
     <|> floatType
     <|> fixedType
     <|> boolType
     <|> voidType
     <|> accountType
     <|> assetType
     <|> contractType
     <|> sigType
     <|> msgType
     <|> dateType
     <|> timedeltaType
     <|> enumType
     <|> collectionType
     <?> "type"

intType :: Parser Type
intType = TInt <$ try (reserved Token.int)

floatType :: Parser Type
floatType = TFloat <$ try (reserved Token.float)

fixedType :: Parser Type
fixedType = do
    prec <- prec1
        <|> prec2
        <|> prec3
        <|> prec4
        <|> prec5
        <|> prec6
    pure $ TFixed prec
  where
    prec1 = Prec1 <$ try (reserved Token.fixed1)
    prec2 = Prec2 <$ try (reserved Token.fixed2)
    prec3 = Prec3 <$ try (reserved Token.fixed3)
    prec4 = Prec4 <$ try (reserved Token.fixed4)
    prec5 = Prec5 <$ try (reserved Token.fixed5)
    prec6 = Prec6 <$ try (reserved Token.fixed6)

boolType :: Parser Type
boolType = TBool <$ try (reserved Token.bool)

voidType :: Parser Type
voidType = TVoid <$ try (reserved Token.void)

accountType :: Parser Type
accountType = TAccount <$ try (reserved Token.account)

assetType :: Parser Type
assetType = do
    atype <- parseAssetBinary
         <|> parseAssetDiscrete
         <|> parseAssetFrac
    pure $ TAsset atype
  where
    parseAssetBinary   = TBinary <$ try (reserved Token.assetBin)
    parseAssetDiscrete = TDiscrete <$ try (reserved Token.assetDis)
    parseAssetFrac     =
      fmap TFractional $
            (Prec1 <$ try (reserved Token.assetFrac1))
        <|> (Prec2 <$ try (reserved Token.assetFrac2))
        <|> (Prec3 <$ try (reserved Token.assetFrac3))
        <|> (Prec4 <$ try (reserved Token.assetFrac4))
        <|> (Prec5 <$ try (reserved Token.assetFrac5))
        <|> (Prec6 <$ try (reserved Token.assetFrac6))

contractType :: Parser Type
contractType = TContract <$ try (reserved Token.contract)

sigType :: Parser Type
sigType = TSig <$ try (reserved Token.sig)

msgType :: Parser Type
msgType = TMsg <$ try (reserved Token.msg)

dateType :: Parser Type
dateType = TDateTime <$ try (reserved Token.datetime)

timedeltaType :: Parser Type
timedeltaType = TTimeDelta <$ try (reserved Token.timedelta)

enumType :: Parser Type
enumType = TEnum <$> try (reserved Token.enum *> Lexer.name)

collectionType :: Parser Type
collectionType =
  fmap TColl $  mapType
            <|> setType

mapType :: Parser TCollection
mapType = do
  try (reserved Token.map)
  a <- symbol "<" *> type_
  lexeme comma
  b <- type_ <* symbol ">"
  pure (TMap a b)

setType :: Parser TCollection
setType = do
  try (reserved Token.set)
  TSet <$> (symbol "<" *> type_ <* symbol ">")

-------------------------------------------------------------------------------
-- Definitions
-------------------------------------------------------------------------------

def :: Parser Def
def = try localDef
  <|> try globalDef
  <|> globalDefNull
  <|> localDefNull
  <?> "definition"

localDef :: Parser Def
localDef = do
  try $ reserved Token.local
  typ <- type_
  id <- name
  reservedOp Token.assign
  lexpr <- expr
  return $ LocalDef typ id lexpr
 <?> "local definition"

localDefNull :: Parser Def
localDefNull = do
  optional $ reserved Token.local
  typ <- type_
  Located loc id <- locName
  return $ LocalDefNull typ (Located loc id)
 <?> "local definition"

globalDef :: Parser Def
globalDef = do
  optional $ reserved Token.global
  typ <- type_
  role <- accessRestriction
  id <- name
  reservedOp Token.assign
  lexpr <- expr
  return $ GlobalDef typ role id lexpr
 <?> "global definition"

globalDefNull :: Parser Def
globalDefNull = do
  optional $ reserved Token.global
  typ <- type_
  role <- accessRestriction
  Located loc id <- locName
  return $ GlobalDefNull typ role (Located loc id)
 <?> "global definition"

-------------------------------------------------------------------------------
-- Label Annotations
-------------------------------------------------------------------------------

labelChar :: Parser Char
labelChar = alphaNum <|> (oneOf $ "_")
  <?> "label"

labelIdent :: Parser Label
labelIdent = do
  label <- many1 labelChar
  pure (Label (T.pack label))
 <?> "label identifier"

tInitial :: Parser Label
tInitial = do
  reserved Token.initial
  pure (Label Token.initial)
 <?> "initial"

tTerminal :: Parser Label
tTerminal = do
  reserved Token.terminal
  pure (Label Token.terminal)
 <?> "terminal"

transitionTag :: Parser GraphLabel
transitionTag = (
      try (char '@' >> Main <$> tInitial)
  <|> try (char '@' >> Main <$> tTerminal)
  <|>     (char '@' >> (Main <$> Lexer.label))
  )
  <?> "annotation"

subtransitionTag :: Parser GraphLabel
subtransitionTag = (
      try (atat >> Subg <$> tInitial)
  <|> try (atat >> Subg <$> tTerminal)
  <|>     (atat >> Subg <$> Lexer.label)
  )
  <?> "annotation"
 where
  atat = char '@' >> char '@'

accessRestriction :: Parser AccessRestriction
accessRestriction = (
    try (RoleAnyOf <$> braces (commaSep1 expr))
    <|> pure RoleAny
    )
    <?> "variable access control"

-------------------------------------------------------------------------------
-- Methods & Helper Functions
-------------------------------------------------------------------------------

arg :: Parser Arg
arg = Arg <$> type_ <*> locName
   <?> "argument"

method :: Parser Method
method = do
  tag <- try (transitionTag <|> subtransitionTag)
  access <- accessRestriction
  nm <- name
  args <- parens $ commaSep arg
  body <- block
  return $ Method tag access nm args body
 <?> "method"

helper :: Parser Helper
helper =  Helper
      <$> Lexer.locName
      <*> parens (commaSep arg)
      <*> block

-------------------------------------------------------------------------------
-- Exprs
-------------------------------------------------------------------------------

binOp :: BinOp -> Expr.Assoc -> Operator Text () Identity LExpr
binOp nm = Expr.Infix (locBinOp nm)

locBinOp :: BinOp -> Parser (LExpr -> LExpr -> LExpr)
locBinOp nm = do
  loc <- location
  opName <- mkLocated $ op nm
  return $ \le1 le2 ->
    Located loc $ EBinOp opName le1 le2

binOpTry :: BinOp -> BinOp -> Expr.Assoc -> Operator Text () Identity LExpr
binOpTry nm nm2 = Expr.Infix (locBinOpTry nm nm2)

locBinOpTry :: BinOp -> BinOp -> Parser (LExpr -> LExpr -> LExpr)
locBinOpTry nm nm2 = do
  loc <- location
  opName <- mkLocated $ try (op nm) <|> op nm2
  return $ \le1 le2 ->
    Located loc $ EBinOp opName le1 le2

unOpExpr :: UnOp -> Operator Text () Identity LExpr
unOpExpr nm = Expr.Prefix (locUnOp nm)

locUnOp :: UnOp -> Parser (LExpr -> LExpr)
locUnOp nm = do
  loc <- location
  opName <- mkLocated $ unOp nm
  return $ \le ->
    Located loc $ EUnOp opName le

opTable :: OperatorTable Text () Identity LExpr
opTable =
  [ [ binOp Mul Expr.AssocLeft ]
  , [ binOp Add Expr.AssocLeft
    , binOp Sub Expr.AssocLeft
    , binOp Div Expr.AssocLeft
    ]
  , [ binOp And Expr.AssocLeft
    , binOp Or Expr.AssocLeft
    ]
  , [ binOp Equal Expr.AssocLeft
    , binOp NEqual Expr.AssocLeft
    , binOpTry LEqual Lesser Expr.AssocLeft
    , binOpTry GEqual Greater Expr.AssocLeft
    , binOp Greater Expr.AssocLeft
    ]
  , [ unOpExpr Not
    ]
  ]

op :: BinOp -> Parser BinOp
op oper = symbol (Lexer.opToken oper) >> pure oper
  <?> "binary operator"

unOp :: UnOp -> Parser UnOp
unOp oper = symbol (Lexer.unOpToken oper) >> pure oper
  <?> "unary operator"

expr :: Parser LExpr
expr = buildExpressionParser opTable locExpr
  where
    -- Expressions without locations and binary and unary ops.
    nonLocExpr :: Parser Expr
    nonLocExpr =  assignExpr
              <|> beforeExpr
              <|> afterExpr
              <|> betweenExpr
              <|> ifElseExpr
              <|> caseExpr
              <|> callExpr
              <|> litExpr
              <|> varExpr

    -- Expressions without binary/unary operations or expressions with
    -- parentheses.
    locExpr :: Parser LExpr
    locExpr =  mkLocated nonLocExpr
           <|> parensLExpr

parensLExpr :: Parser LExpr
parensLExpr = parens expr

litExpr :: Parser Expr
litExpr = ELit <$> locLit
 <?> "literal"

varExpr :: Parser Expr
varExpr = EVar <$> locName
 <?> "variable"

assignExpr :: Parser Expr
assignExpr = do
  var <- try $ name <* reservedOp Token.assign
  lexpr <- expr
  return $ EAssign var lexpr
 <?> "assign statement"

callExpr :: Parser Expr
callExpr = do
  lnm@(Located _ nm) <-
    try $ Lexer.locName <* symbol Token.lparen
  let fname = case lookupPrim nm of
        Nothing  -> Right lnm
        Just pop -> Left pop
  args <- commaSep expr <* symbol Token.rparen
  return $ ECall fname args
 <?> "call statement"

ifElseExpr :: Parser Expr
ifElseExpr = do
  try $ reserved Token.if_
  cond <- parensLExpr
  e1 <- block
  e2 <- elseBranch
  return $ EIf cond e1 e2
 <?> "if statement"
  where
    -- Parse either an else block, or a noop expr
    elseBranch :: Parser LExpr
    elseBranch = do
      loc <- location
      Text.Parsec.option
        (Located loc $ ENoOp)
        (try (reserved Token.else_) *> block
          <?> "else statement")

beforeExpr :: Parser Expr
beforeExpr = do
  try $ reserved Token.before
  dt <- parensLExpr
  e <- block
  return $ EBefore dt e
 <?> "before guard statement"

afterExpr :: Parser Expr
afterExpr = do
  try $ reserved Token.after
  dt <- parensLExpr
  e <- block
  return $ EAfter dt e
 <?> "after guard statement"

betweenExpr :: Parser Expr
betweenExpr = do
  try $ reserved Token.between
  start <- symbol Token.lparen *> expr
  lexeme comma
  end <- expr <* symbol Token.rparen
  e <- block
  return $ EBetween start end e
 <?> "between guard statement"

caseExpr :: Parser Expr
caseExpr = do
  try $ reserved Token.case_
  scrutinee <- parensLExpr
  symbol Token.lbrace
  matches <- many1 (Match <$> pattern_
                          <* reserved Token.rarrow
                          <*> (block <|> expr)
                          <* semi)
  symbol Token.rbrace
  return $ ECase scrutinee matches

pattern_ :: Parser LPattern
pattern_ = do
  loc <- location
  Located loc . PatLit <$> try (symbol "`" *> Lexer.enumConstr)

-- | Parses 0 or more expressions delimited by ';'
block :: Parser LExpr
block = (braces $ do
  loc <- location
  eseq loc <$> (many $ expr <* semi))
 <?> "expression block"

-------------------------------------------------------------------------------
-- Transitions
-------------------------------------------------------------------------------

initial :: Parser Transition
initial = do
  reserved Token.initial
  pure $ Initial
 <?> "initial"

terminal :: Parser Transition
terminal = do
  reserved Token.terminal
  pure $ Terminal
 <?> "terminal"

transition_label :: Parser Transition
transition_label =
      initial
  <|> terminal
  <|> (Step <$> Lexer.label)
  <?> "label"

transition :: Parser Transition
transition = do
  reserved Token.transition;
  from <- transition_label
  reserved Token.rarrow
  to   <- transition_label
  return $ Arrow from to
 <?> "transition"

-------------------------------------------------------------------------------
-- Enumeration type definition
-------------------------------------------------------------------------------

enumDef :: Parser EnumDef
enumDef = do
  reserved Token.enum
  lname <- Lexer.locName
  constrs <- braces $ commaSep1 Lexer.locEnumConstr
  return $ EnumDef lname constrs

-------------------------------------------------------------------------------
-- Script
-------------------------------------------------------------------------------

script :: Parser Script
script = do
  enums <- endBy enumDef semi
  defns <- endBy def semi
  graph <- endBy transition semi
  methods <- many method
  helpers <- many helper
  return $ Script enums defns graph methods helpers
 <?> "script"

-------------------------------------------------------------------------------
-- Errors
-------------------------------------------------------------------------------

data ParseErrInfo = ParseErrInfo
  { line         :: Int
  , lineContents :: Text
  , lineAfterContents :: Text
  , column       :: Int
  , errMsg       :: Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

mkParseErrInfo :: Text -> ParseError -> ParseErrInfo
mkParseErrInfo input perr = ParseErrInfo
  { line   = line
  , lineContents = T.unlines $ take 2 $ drop (line - 2) lines
  , lineAfterContents = T.unlines $ take 2 $ drop (line ) lines
  , column = column
  , errMsg = show perr
  }
  where
    line = sourceLine $ errorPos perr
    column = sourceColumn $ errorPos perr
    lines = T.lines input

instance Pretty ParseErrInfo where
  ppr ParseErrInfo{..} =
    text (toS lineContents)
    <> text (toS $ T.replicate (column - 1) "-") <> "^"
    <$$> text (toS lineAfterContents)
    <$$> (text $ toS errMsg )
