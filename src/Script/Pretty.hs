{-|

The pretty printer for FCL syntax, types and values.

-}

module Script.Pretty (
  Doc,
  Pretty(..),

  -- ** Printing
  Script.Pretty.print,
  render,
  pprRender,
  prettyPrint,
  printList,

  -- ** Leijen.Text exports
  (PP.<>),
  (PP.<+>),
  (PP.<$$>),
  (PP.<//>),
  PP.line,
  PP.sep,
  PP.hang,
  PP.indent,
  PP.text,
  PP.lbrace,
  PP.rbrace,
  PP.nest,
  PP.hcat,
  PP.hsep,
  PP.vcat,
  PP.vsep,
  PP.dquotes,
  PP.squotes,
  PP.softbreak,

  -- ** Utils
  (<$$+>),
  (<$$$>),
  (<$$$+>),
  append,
  listOf,
  tupleOf,
  commafy,
  semify,
  parensIf,
  parens,
  ppMaybe,
  ppshow,
  assign,
  token,
  spaced,
  punctuate,

  -- ** Testing
  testPpr,
  testPprList,
) where

import Protolude hiding ((<>), (<$>))
import Text.PrettyPrint.Leijen.Text as PP hiding (Pretty, equals)

import qualified Script.Token as Token

pprRender :: Pretty a => a -> LText
pprRender = render . ppr

prettyPrint :: Pretty a => a -> Text
prettyPrint = toS . pprRender

-------------------------------------------------------------------------------
-- Base Class
-------------------------------------------------------------------------------

class Pretty p where
  ppr :: p -> Doc
  {-# MINIMAL ppr #-}

instance Pretty Text where
  ppr = text . fromStrict

instance Pretty LText where
  ppr = text

instance Pretty ByteString where
  ppr = text . toS

instance Pretty [Char] where
  ppr = text . toS

instance Pretty Int where
  ppr = int

instance Pretty Int64 where
  ppr = int . fromIntegral

instance Pretty Integer where
  ppr = integer

instance Pretty Double where
  ppr = double

instance Pretty Float where
  ppr = float

instance Pretty Bool where
  ppr = PP.bool

instance Pretty Doc where
  ppr = identity

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  ppr (Left a)  = ppr a
  ppr (Right b) = ppr b

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

(<$$+>) :: Doc -> Doc -> Doc
d1 <$$+> d2 = d1 <$$> indent 3 d2

(<$$$>) :: Doc -> Doc -> Doc
d1 <$$$> d2 = d1 <> linebreak <> linebreak <> d2

(<$$$+>) :: Doc -> Doc -> Doc
d1 <$$$+> d2 = d1 <> linebreak <> linebreak <> indent 3 d2

listOf :: Pretty a => [a] -> Doc
listOf xs = token Token.lbrace <> commafy (fmap ppr xs) <> token Token.rbrace

tupleOf :: Pretty a => [a] -> Doc
tupleOf xs = token Token.lparen <> commafy (fmap ppr xs) <> token Token.rparen

commafy :: [Doc] -> Doc
commafy = hsep . punctuate comma

append :: Doc -> Doc -> Doc
append = flip (<>)

semify :: Doc -> Doc
semify = flip (<>) semi

parensIf :: Bool -> Doc -> Doc
parensIf True = parens
parensIf False = identity

ppMaybe :: Pretty a => Maybe a -> Doc
ppMaybe = maybe mempty ppr

ppshow :: Show a => a -> Doc
ppshow x = ppr (show x :: Text)

assign :: Doc -> Doc -> Doc
assign doc1 doc2 = doc1 <+> token Token.assign <+> doc2 <> semi

-- | Put soft line breaks between elements.
spaced :: [Doc] -> [Doc]
spaced = intersperse softbreak

token :: Text -> Doc
token = text . fromStrict

render :: Doc -> LText
render = displayT . renderPretty 1 120

print :: Pretty a => a -> LText
print = render . ppr

printList :: Pretty a => [a] -> LText
printList xs = render (vcat (fmap ppr xs))

-------------------------------------------------------------------------------
-- Testing
-------------------------------------------------------------------------------

testPpr :: Pretty a => a -> IO ()
testPpr = putStrLn . render . ppr

testPprList :: LText
testPprList = render $ ppr $ listOf [True, False, True]
