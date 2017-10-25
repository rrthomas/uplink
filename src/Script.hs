{-

Core AST for the FCL core language.

-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Script (
  -- ** Syntax
  Script(..),
  Expr(..),
  Method(..),
  Def(..),
  Arg(..),
  Lit(..),
  BinOp(..),
  UnOp(..),
  GraphLabel(..),

  -- ** Location info
  Loc(..),
  Located(..),
  LExpr,
  LBinOp,
  LUnOp,
  LLit,
  LName,
  LType,

  -- ** Values
  Script.Value(..),
  evalLit,
  evalLLit,

  -- ** Name
  Name(..),

  -- ** State Labels
  Label(..),
  Transition(..), -- reexport

  -- ** DateTime
  DateTime(..),
  TimeDelta(..),

  -- ** Types
  TVar(..),
  Type(..),

  -- ** Helpers
  eseq,
  unseq,
  argtys,
  argtys',
  argLits,
  unLoc,
  at,
  methodNames,
  mapType,
  emptyScript,

  -- ** Pretty Printing
  ppScript,

  module Script.Fixed,

) where

import Protolude hiding (put, get, (<>), show, Show, putByteString)
import Prelude (show, Show(..))

import Script.Fixed
import Script.Graph
import Script.Pretty
import qualified Script.Pretty as Pretty

import SafeInteger
import SafeString
import Address (Address, rawAddr)
import qualified Hash
import qualified Script.Token as Token

import qualified Datetime as D
import qualified Datetime.Types as DT

import Data.Fixed
import Data.Time.Clock
import Data.Hashable (Hashable)
import Data.String (IsString(..))
import Data.Time.Calendar (Day(..))
import Data.Serialize (Serialize(..))
import Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.Text as T

-------------------------------------------------------------------------------
-- Core Language
-------------------------------------------------------------------------------

data Loc
  = NoLoc
  | Loc { line :: Int, col :: Int }
  deriving (Eq, Show, Ord, Generic, NFData, Serialize, Hash.Hashable)

data Located a = Located
  { located :: Loc
  , locVal  :: a
  } deriving (Generic, Hash.Hashable)

instance Show a => Show (Located a) where
  show (Located _ a) = show a
  showsPrec d (Located _ a) = showsPrec d a

instance NFData a => NFData (Located a)

instance Eq a => Eq (Located a) where
  (==) l1 l2 = locVal l1 == locVal l2

instance Ord a => Ord (Located a) where
  compare l1 l2 = locVal l1 `compare` locVal l2

-- | Attach location information to a type
type LExpr = Located Expr
type LLit  = Located Lit
type LType = Located Type
type LName = Located Name
type LBinOp = Located BinOp
type LUnOp  = Located UnOp

-- | Variable names
newtype Name = Name { unName :: Text }
  deriving (Eq, Show, Ord, Generic, Hashable, Hash.Hashable, NFData, ToJSON)

-- | Datetime literals
newtype DateTime = DateTime { unDateTime :: DT.Datetime }
  deriving (Eq, Ord, Show, Generic, NFData, Serialize, Hashable, ToJSON, FromJSON)

newtype TimeDelta = TimeDelta { unTimeDelta :: DT.Delta }
   deriving (Eq, Ord, Show, Generic, NFData, Serialize, Hashable, ToJSON, FromJSON)

instance Hash.Hashable TimeDelta where
  toHash = Hash.toHash . (toS :: [Char] -> ByteString) . show

instance Hash.Hashable DateTime where
  toHash = Hash.toHash . (toS :: [Char] -> ByteString) . show

data Expr
  = ESeq     LExpr LExpr           -- ^ Sequencing
  | ELit     LLit                  -- ^ Literal
  | EVar     LName                 -- ^ Variable reference
  | ERet     LExpr                 -- ^ Return a value
  | EBinOp   LBinOp LExpr LExpr    -- ^ Basic Binary ops on Integers
  | EUnOp    LUnOp  LExpr          -- ^ Basic unary ops
  | EIf      LExpr LExpr LExpr     -- ^ Conditional
  | EBefore  LExpr LExpr           -- ^ Time guard
  | EAfter   LExpr LExpr           -- ^ Time guard
  | EBetween LExpr LExpr LExpr     -- ^ Time guard
  | EAssign  Name  LExpr           -- ^ Reference update
  | ECall    Name  [LExpr]         -- ^ Function call
  | ENoOp                          -- ^ Empty method body
  deriving (Eq, Ord, Show, Generic, Hash.Hashable, NFData)

data BinOp
  = Add     -- ^ Addition
  | Sub     -- ^ Subtraction
  | Mul     -- ^ Multiplication
  | Div     -- ^ Division, modulo for integers
  | And     -- ^ Logical conjunction
  | Or      -- ^ Logical disjunction
  | Equal   -- ^ Equality
  | NEqual  -- ^ Unequality
  | LEqual  -- ^ Lesser equal
  | GEqual  -- ^ Greater equal
  | Lesser  -- ^ Lesser
  | Greater -- ^ Greater
  deriving (Eq, Ord, Show, Generic, Hash.Hashable, NFData)

data UnOp = Not -- ^ Logical negation
  deriving (Eq, Ord, Show, Generic, Hash.Hashable, NFData)

-- | Literal representing Value
data Lit
  = LInt      Int64
  | LFloat    Double
  | LFixed    FixedN
  | LBool     Bool
  | LState    Label
  | LAddress  Address
  | LAccount  Address
  | LAsset    Address
  | LContract Address
  | LMsg      SafeString
  | LSig      (SafeInteger,SafeInteger)
  | LDateTime  DateTime
  | LTimeDelta TimeDelta
  | LUndefined
  | LVoid
  deriving (Eq, Ord, Show, Generic, Hash.Hashable, NFData)

-- | Values in which literals are evaluated to
data Value
  = VInt Int64             -- ^ Integral types
  | VCrypto SafeInteger    -- ^ Homomorphic encrypted integral types
  | VFloat Double          -- ^ Floating types
  | VFixed FixedN          -- ^ Fixed point types
  | VBool Bool             -- ^ Boolean value
  | VAddress Address       -- ^ Address
  | VAccount Address       -- ^ Account Address
  | VAsset Address         -- ^ Asset Address
  | VContract Address      -- ^ Contract Address
  | VMsg SafeString        -- ^ Msgs (ASCII)
  | VSig (SafeInteger,SafeInteger) -- ^ ESDSA Sig
  | VVoid                  -- ^ Void
  | VDateTime DateTime     -- ^ A datetime with a timezone
  | VTimeDelta TimeDelta   -- ^ A difference in time
  | VState Label           -- ^ Named state label
  | VUndefined             -- ^ Undefined
  deriving (Eq, Ord, Show, Generic, NFData, Serialize, Hashable, Hash.Hashable)

-- | Type variables used in inference
newtype TVar = TV Text
  deriving (Eq, Ord, Show, Generic, NFData, Hash.Hashable, Hashable)

-- | Core types for FCL
data Type
  = TError          -- ^ (Internal) Error branch in typechecker
  | TVar TVar       -- ^ (Internal) Type variable in inference
  | TRef Type       -- ^ (Internal) Variable type
  | TCrypto Type    -- ^ (Internal) Local variables
  | TAddress        -- ^ (Internal) Type of addresses
  | TInt            -- ^ Type of 64 bit integers
  | TFloat          -- ^ Type of double precision floats
  | TFixed PrecN    -- ^ Type of double precision floats
  | TBool           -- ^ Type of booleans
  | TAccount        -- ^ Type of account addresses
  | TAsset          -- ^ Type of asset addresses
  | TContract       -- ^ Type of contract addresses
  | TMsg            -- ^ Type of messages
  | TSig            -- ^ Type of ECDSA signature
  | TVoid           -- ^ Type of void
  | TDateTime       -- ^ DateTime with Timezone
  | TTimeDelta      -- ^ Type of difference in time
  | TState          -- ^ Contract state
  | TAny            -- ^ Polymorphic type
  deriving (Eq, Ord, Show, Generic, NFData, Hash.Hashable, Hashable)

-- | Function argument
data Arg
  = Arg { argType :: Type, argName ::  LName }
  deriving (Eq, Ord, Show, Generic, NFData, Hash.Hashable)

-- | Graph labels
data GraphLabel
  = Main Label -- ^ Main graph
  | Subg Label -- ^ Subgraph ( novation, owner transfer, etc )
  deriving (Eq, Ord, Show, Generic, NFData, Hash.Hashable)

-- | Method
data Method = Method
  { methodTag  :: GraphLabel
  , methodName :: Name
  , methodArgs :: [Arg]
  , methodBody :: LExpr
  } deriving (Eq, Show, Generic, NFData, Hash.Hashable)

-- | Definition
data Def
  = GlobalDef Type Name LLit
  | GlobalDefNull Type LName
  | LocalDef Type Name LLit
  | LocalDefNull Type LName
  deriving (Eq, Show, Generic, NFData, Hash.Hashable)

-- | Script
data Script = Script
  { scriptDefs        :: [Def]
  , scriptTransitions :: [Transition]
  , scriptMethods     :: [Method]
  } deriving (Eq, Show, Generic, NFData, Hash.Hashable)

emptyScript :: Script
emptyScript = Script
  { scriptDefs        = []
  , scriptTransitions = []
  , scriptMethods     = []
  }

-- | Method args names and types
argtys :: Method -> [(Name,Type)]
argtys m = [(unLoc lnm, ty) | Arg ty lnm <- methodArgs m ]

argtys' :: Method -> [Type]
argtys' = map snd . argtys

-- | Method argument literals
argLits :: [Expr] -> [LLit]
argLits (ELit n : xs) = n : argLits xs
argLits (_ : xs) = argLits xs
argLits [] = []

-- | Method names
methodNames :: Script -> [Name]
methodNames = fmap methodName . scriptMethods

-- | Remove position information on an element
unLoc :: Located a -> a
unLoc (Located loc a) = a

-- | Put location information on an element
at :: a -> Loc -> Located a
at = flip Located

-- | Unroll a sequence of statements into a list.
unseq :: LExpr -> [LExpr]
unseq (Located l e) = case e of
  ESeq a b       -> unseq a ++ unseq b
  EIf cond tr fl -> unseq tr ++ unseq fl
  _              -> [Located l e]

-- | Roll a list of expressions into a sequence.
eseq :: Loc -> [LExpr] -> LExpr
eseq loc es = case es of
  []     -> Located loc ENoOp
  [x]    -> x
  (x:xs) -> Located loc $
    ESeq x $ eseq (located x) xs

mapFixedType :: FixedN -> PrecN
mapFixedType = \case
  Fixed1 _ -> Prec1
  Fixed2 _ -> Prec2
  Fixed3 _ -> Prec3
  Fixed4 _ -> Prec4
  Fixed5 _ -> Prec5
  Fixed6 _ -> Prec6

-- | Map values into respective frontend types
mapType :: Script.Value -> Type
mapType = \case
 VInt {}      -> TInt
 VCrypto {}   -> TCrypto TInt
 VFloat {}    -> TFloat
 VFixed f     -> TFixed (mapFixedType f)
 VBool {}     -> TBool
 VAccount {}  -> TAccount
 VAsset {}    -> TAsset
 VContract {} -> TContract
 VAddress {}  -> TAddress
 VVoid        -> TVoid
 VMsg {}      -> TMsg
 VSig {}      -> TSig
 VDateTime {} -> TDateTime
 VTimeDelta {} -> TTimeDelta
 VState {}    -> TState
 VUndefined   -> TAny -- XXX: ugly hack, nothing else to do here

-------------------------------------------------------------------------------
-- Serialization
-------------------------------------------------------------------------------

instance IsString Name where
  fromString = Name . toS

instance Serialize Name where
  put (Name nm) = put (encodeUtf8 nm)
  get = Name . decodeUtf8 <$> get

instance IsString TVar where
  fromString = TV . toS

instance Serialize TVar where
  put (TV tv) = put (encodeUtf8 tv)
  get = TV . decodeUtf8 <$> get

instance (Serialize a) => Serialize (Located a) where
  put (Located loc x) = put (loc,x)
  get = uncurry Located <$> get

instance Serialize Lit where
instance Serialize Def where
instance Serialize Arg where
instance Serialize Type where
instance Serialize Expr where
instance Serialize BinOp where
instance Serialize UnOp where
instance Serialize Method where
instance Serialize Script where
instance Serialize GraphLabel where

-------------------------------------------------------------------------------
-- Pretty Printer
-------------------------------------------------------------------------------

instance Pretty TimeDelta where
  ppr (TimeDelta d) = ppr $ DT.displayDelta d

instance Pretty Loc where
  ppr NoLoc     = "No location info"
  ppr (Loc l c) = "Line" <+> append "," (ppr l) <+> "Column" <+> ppr c

instance (Pretty a) => Pretty (Located a) where
  ppr (Located _ x) = ppr x

instance Pretty Name where
  ppr (Name nm) = ppr nm

instance Pretty Expr where
  ppr = \case
    ENoOp            -> mempty
    ESeq e e'        -> vsep [ppr' e, ppr e']
                      where
                        ppr' (Located _ expr) =
                          case expr of
                            ENoOp     -> ppr expr
                            otherwise -> semify $ ppr expr

    ELit lit         -> ppr lit
    EVar nm          -> ppr nm
    ERet e           -> token Token.return <+> ppr e
    EAssign nm e     -> ppr nm <+> token Token.assign <+> ppr e
    EUnOp nm e       -> parens $ ppr nm <> ppr e
    EBinOp nm e e'   -> parens $ ppr e <+> ppr nm <+> ppr e'
    ECall nm es      -> hcat [ppr nm, tupleOf (map ppr es)]
    EBefore dt e     -> token Token.before <+> parens (ppr dt) <+> lbrace
                   <$$> indent 2 ((if e' == ENoOp then identity else semify) (ppr e'))
                   <$$> rbrace
                      where
                        Located _ e' = e

    EAfter dt e      -> token Token.after <+> parens (ppr dt) <+> lbrace
                   <$$> indent 2 ((if e' == ENoOp then identity else semify) (ppr e'))
                   <$$> rbrace
                      where
                        Located _ e' = e

    EBetween d1 d2 e -> token Token.between <+> tupleOf (map ppr [d1,d2]) <+> lbrace
                   <$$> indent 2 ((if e' == ENoOp then identity else semify) (ppr e'))
                   <$$> rbrace

                      where
                        Located _ e' = e
    EIf c e1 e2      -> token Token.if_ <+> parensIf True (ppr c) <+> lbrace
                  <$$+> (if e1' == ENoOp then identity else semify) (ppr e1)
                   <$$> (if e2' == ENoOp
                           then mempty
                           else rbrace <+> token Token.else_ <+> lbrace
                          <$$+> semify (ppr e2)
                        )
                   <$$> rbrace
                      where
                        Located _ e1' = e1
                        Located _ e2' = e2

instance Pretty Lit where
  ppr = \case
    LInt int64     -> ppr int64
    LFloat dbl     -> ppr dbl
    LFixed mfix    -> ppr mfix
    LBool bool     -> ppr bool
    LMsg msg       -> dquotes $ ppr msg
    LAddress addr  -> squotes $ ppr $ rawAddr addr
    LAccount addr  -> squotes $ ppr $ rawAddr addr
    LAsset addr    -> squotes $ ppr $ rawAddr addr
    LContract addr -> squotes $ ppr $ rawAddr addr
    LSig (r,s)     -> tupleOf [ppr r, ppr s]
    LVoid          -> token Token.void
    LState label   -> token Token.colon <> ppr label
    LUndefined     -> dquotes "Undefined" -- Not in syntax
    LDateTime dt   -> dquotes $ ppr $ (DT.formatDatetime (unDateTime dt) :: [Char])
    LTimeDelta d   -> dquotes $ ppr d

instance Pretty Type where
  ppr = \case
    TInt        -> token Token.int
    TFloat      -> token Token.float
    TFixed prec -> ppr prec
    TBool       -> token Token.bool
    TAddress    -> token Token.contract
    TAccount    -> token Token.account
    TAsset      -> token Token.asset
    TContract   -> token Token.contract
    TVoid       -> token Token.void
    TSig        -> token Token.sig
    TMsg        -> token Token.msg
    TError      -> text $ "<error-type>"  -- Not in syntax
    TVar (TV v) -> text $ toSL v
    TRef t      -> hcat ["ref <", ppr t, ">"]  -- Not in syntax
    TCrypto t   -> hsep [ppr Token.local, ppr t]
    TDateTime   -> token Token.datetime
    TTimeDelta  -> token Token.timedelta
    TState      -> token Token.state
    TAny        -> token Token.any

instance Pretty Script.Value where
  ppr = \case
    VInt n       -> ppr n
    VCrypto n    -> ppr n
    VFloat n     -> ppr n
    VFixed n     -> ppr n
    VBool n      -> ppr n
    VAddress n   -> ppr n
    VAccount n   -> ppr n
    VAsset n     -> ppr n
    VContract n  -> ppr n
    VMsg n       -> ppr n
    VSig (r,s)   -> ppr r <> "," <> ppr s
    VVoid        -> token Token.void
    VDateTime dt -> ppr (DT.formatDatetime (unDateTime dt) :: [Char])
    VTimeDelta d -> ppr d
    VState n     -> ppr n
    VUndefined   -> "undefined"

instance Pretty Arg where
  ppr (Arg typ name) = ppr typ <+> ppr name

instance Pretty BinOp where
  ppr = \case
    Add     -> token Token.add
    Sub     -> token Token.sub
    Div     -> token Token.div
    Mul     -> token Token.mult
    And     -> token Token.and
    Or      -> token Token.or
    Equal   -> token Token.equal
    NEqual  -> token Token.nequal
    LEqual  -> token Token.lequal
    GEqual  -> token Token.gequal
    Lesser  -> token Token.lesser
    Greater -> token Token.greater

instance Pretty UnOp where
  ppr Not = token Token.not

instance Pretty Method where
  ppr (Method tag name args (Located _ body)) =
    ppr tag <$$> ppr name <> tupleOf (map ppr args) <+>
      case body of
        ENoOp -> lbrace <+> rbrace
        e@(ESeq _ _) -> lbrace
          <$$> indent 2 (semify $ ppr e)
          <$$> rbrace
        other -> lbrace <$$> indent 2 (semify (ppr other)) <$$> rbrace

instance Pretty Def where
  ppr = \case
    GlobalDefNull typ (Located _ name) -> hsep [token Token.global, ppr typ, ppr name] <> token Token.semi
    LocalDefNull typ (Located _ name)  -> hsep [token Token.local, ppr typ, ppr name] <> token Token.semi
    GlobalDef typ name lit             -> hsep [token Token.global, ppr typ, ppr name `assign` ppr lit]
    LocalDef typ name lit              -> hsep [token Token.local, ppr typ, ppr name `assign` ppr lit]

instance Pretty Script where
  ppr (Script defns graph methods) = vsep
    [ vsep (map ppr defns)
    , Pretty.softbreak
    , vsep (map (semify . ppr) graph)
    , Pretty.softbreak
    , vsep (spaced (map ppr methods))
    ]

instance Pretty GraphLabel where
  ppr = \case
    Main lab -> token Token.at <> ppr lab
    Subg lab -> token Token.atat <> ppr lab

ppScript :: Script -> LText
ppScript = render . ppr

-------------------------------------------------------------------------------
-- Map Literals
-------------------------------------------------------------------------------

evalLit :: Lit -> Script.Value
evalLit lit = case lit of
  LInt n      -> VInt n
  LFloat n    -> VFloat n
  LFixed n    -> VFixed n
  LVoid       -> VVoid
  LBool n     -> VBool n
  LAccount n  -> VAccount n
  LAsset n    -> VAsset n
  LContract n -> VContract n
  LAddress n  -> VAddress n
  LMsg n      -> VMsg n
  LSig n      -> VSig n
  LState n    -> VState n
  LUndefined  -> VUndefined
  LDateTime d -> VDateTime d
  LTimeDelta d -> VTimeDelta d

evalLLit :: LLit -> Script.Value
evalLLit (Located _ lit) = evalLit lit
