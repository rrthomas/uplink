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
  Pattern(..),
  Match(..),
  Method(..),
  EnumDef(..),
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
  LEnumConstr,
  LPattern,

  -- ** Values
  Script.Value(..),
  evalLit,
  evalLLit,

  -- ** Name
  Name(..),

  -- ** Enum constructor
  EnumConstr(..),

  -- ** State Labels
  Label(..),
  Transition(..), -- reexport

  -- ** DateTime
  DateTime(..),
  TimeDelta(..),

  -- ** Types
  TVar(..),
  TAsset(..),
  Type(..),

  -- ** Helpers
  EnumInfo(..),
  createEnumInfo,

  eseq,
  unseq,
  argtys,
  argtys',
  argLits,
  unLoc,
  at,
  methodNames,
  lookupMethod,
  mapType,
  emptyScript,

  -- ** Pretty Printing
  ppScript,

) where

import Protolude hiding (put, get, (<>), show, Show, putByteString, Type)
import Prelude (show, Show(..))

import Control.Monad (fail)

import Fixed
import Script.Graph
import Script.Pretty
import qualified Script.Pretty as Pretty

import SafeInteger
import SafeString
import Address (Address, rawAddr)
import Asset (AssetType(..))
import qualified Hash
import qualified Script.Token as Token

import qualified Datetime as D
import qualified Datetime.Types as DT

import qualified Data.Binary as B
import Data.Fixed
import Data.Time.Clock
import Data.Hashable (Hashable)
import Data.String (IsString(..))
import Data.Time.Calendar (Day(..))
import Data.Serialize (Serialize(..), encode, decode, putInt8, getInt8)
import Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.Text as T
import qualified Data.Map as Map

import Database.PostgreSQL.Simple.ToField   (ToField(..), Action(..))
import Database.PostgreSQL.Simple.FromField (FromField(..), ResultError(..), returnError)

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

instance Functor Located where
  fmap f (Located l v) = Located l (f v)

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
type LEnumConstr = Located EnumConstr
type LPattern = Located Pattern

-- | Enum constructor.
newtype EnumConstr = EnumConstr { unEnumConstr :: SafeString }
  deriving (Eq, Show, Ord, Generic, Hashable, Hash.Hashable, NFData)

instance ToJSON EnumConstr where
  toJSON = toJSON . unEnumConstr

instance FromJSON EnumConstr where
  parseJSON = fmap EnumConstr . parseJSON

-- | Variable names
newtype Name = Name { unName :: Text }
  deriving (Eq, Show, Ord, Generic, Hashable, Hash.Hashable, NFData, B.Binary)

-- | Datetime literals
newtype DateTime = DateTime { unDateTime :: DT.Datetime }
  deriving (Eq, Ord, Show, Generic, NFData, Serialize, Hashable)

newtype TimeDelta = TimeDelta { unTimeDelta :: DT.Delta }
   deriving (Eq, Ord, Show, Generic, NFData, Serialize, Hashable, ToJSON, FromJSON)

instance Hash.Hashable TimeDelta where
  toHash = Hash.toHash . (toS :: [Char] -> ByteString) . show

instance Hash.Hashable DateTime where
  toHash = Hash.toHash . (toS :: [Char] -> ByteString) . show

data Pattern
  = PatLit EnumConstr
  deriving (Eq, Ord, Show, Generic, Hash.Hashable, NFData)

data Match
  = Match { matchPat :: LPattern
          , matchBody :: LExpr
          }
  deriving (Eq, Ord, Show, Generic, Hash.Hashable, NFData)

data Expr
  = ESeq     LExpr LExpr           -- ^ Sequencing
  | ELit     LLit                  -- ^ Literal
  | EVar     LName                 -- ^ Variable reference
  | EBinOp   LBinOp LExpr LExpr    -- ^ Basic Binary ops on Integers
  | EUnOp    LUnOp  LExpr          -- ^ Basic unary ops
  | EIf      LExpr LExpr LExpr     -- ^ Conditional
  | EBefore  LExpr LExpr           -- ^ Time guard
  | EAfter   LExpr LExpr           -- ^ Time guard
  | EBetween LExpr LExpr LExpr     -- ^ Time guard
  | ECase    LExpr [Match]         -- ^ Case statement
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
  | LConstr EnumConstr
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
  | VEnum EnumConstr       -- ^ Constructor of the given enum type
  | VState Label           -- ^ Named state label
  | VUndefined             -- ^ Undefined
  deriving (Eq, Ord, Show, Generic, NFData, Serialize, Hashable, Hash.Hashable)

-- | Type variables used in inference
data TVar
  = TV  Text -- ^ General type variable
  | TAV Text -- ^ Type variable used for inferring return types of prim ops operating over assets
  deriving (Eq, Ord, Show, Generic, NFData, Hash.Hashable, Hashable)

data TAsset
  = TDiscrete         -- ^ Type of Discrete Assets
  | TBinary           -- ^ Type of Binary Assets
  | TFractional PrecN -- ^ Type of Fractional Assets
  deriving (Eq, Ord, Show, Generic, NFData, Hash.Hashable, Hashable, Serialize)

-- | Core types for FCL
data Type
  = TError          -- ^ (Internal) Error branch in typechecker
  | TVar TVar       -- ^ (Internal) Type variable used in inference
  | TAny            -- ^ (Internal) Polymorphic type
  | TAssetAny       -- ^ (Internal) Polymorphic asset type determining type of holdings in prim ops
  | TRef Type       -- ^ (Internal) Variable type
  | TCrypto Type    -- ^ (Internal) Local variables
  | TAddress        -- ^ (Internal) Type of addresses
  | TInt            -- ^ Type of 64 bit integers
  | TFloat          -- ^ Type of double precision floats
  | TFixed PrecN    -- ^ Type of double precision floats
  | TBool           -- ^ Type of booleans
  | TAccount        -- ^ Type of account addresses
  | TAsset TAsset   -- ^ Type of asset addresses
  | TContract       -- ^ Type of contract addresses
  | TMsg            -- ^ Type of messages
  | TSig            -- ^ Type of ECDSA signature
  | TVoid           -- ^ Type of void
  | TDateTime       -- ^ DateTime with Timezone
  | TTimeDelta      -- ^ Type of difference in time
  | TState          -- ^ Contract state
  | TEnum Name      -- ^ Enumeration type
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
  } deriving (Eq, Ord, Show, Generic, NFData, Hash.Hashable)

-- | Enumeration
data EnumDef = EnumDef
  { enumName :: LName
  , enumConstrs :: [LEnumConstr]
  } deriving (Eq, Ord, Show, Generic, NFData, Hash.Hashable)

-- | Definition
data Def
  = GlobalDef Type Name LExpr
  | GlobalDefNull Type LName
  | LocalDef Type Name LExpr
  | LocalDefNull Type LName
  deriving (Eq, Ord, Show, Generic, NFData, Hash.Hashable)

-- | Script
data Script = Script
  { scriptEnums       :: [EnumDef]
  , scriptDefs        :: [Def]
  , scriptTransitions :: [Transition]
  , scriptMethods     :: [Method]
  } deriving (Eq, Ord, Show, Generic, NFData, Hash.Hashable)

emptyScript :: Script
emptyScript = Script
  { scriptEnums       = []
  , scriptDefs        = []
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

lookupMethod :: Name -> Script -> Maybe Method
lookupMethod nm s =
  find ((==) nm . methodName) (scriptMethods s)

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
  EBefore _ s    -> unseq s
  EAfter _ s     -> unseq s
  EBetween _ _ s -> unseq s
  ECase _ ms     -> concatMap (unseq . matchBody) ms
  _              -> [Located l e]

-- | Roll a list of expressions into a sequence.
eseq :: Loc -> [LExpr] -> LExpr
eseq loc es = case es of
  []     -> Located loc ENoOp
  [x]    -> x
  (x:xs) -> Located loc $
    ESeq x $ eseq (located x) xs

mapFixedType :: FixedN -> Type
mapFixedType fn =
  TFixed $ case fn of
    Fixed1 _ ->  Prec1
    Fixed2 _ ->  Prec2
    Fixed3 _ ->  Prec3
    Fixed4 _ ->  Prec4
    Fixed5 _ ->  Prec5
    Fixed6 _ ->  Prec6

-- | Map types to values, taking enum types into account. Returns
-- @Nothing@ in case of an unknown constructor.
mapType :: EnumInfo -> Script.Value -> Maybe Type
mapType enumInfo (VEnum c) = TEnum <$> Map.lookup c (constrToEnum enumInfo)
mapType _ VInt{} = pure TInt
mapType _ VCrypto{} = pure $ TCrypto TInt
mapType _ VFloat{} = pure TFloat
mapType _ (VFixed f) = pure $ mapFixedType f
mapType _ VBool{} = pure TBool
mapType _ VAccount{} = pure TAccount
mapType _ VAsset{} = pure TAssetAny
mapType _ VContract{}= pure TContract
mapType _ VAddress{} = pure TAddress
mapType _ VVoid = pure TVoid
mapType _ VMsg{} = pure TMsg
mapType _ VSig{} = pure TSig
mapType _ VDateTime{} = pure TDateTime
mapType _ VTimeDelta{} = pure TTimeDelta
mapType _ VState{} = pure TState
mapType _ VUndefined = pure TAny

data EnumInfo = EnumInfo
  { constrToEnum :: Map EnumConstr Name
  , enumToConstrs :: Map Name [EnumConstr]
  }

-- | Create the dictionaries for the constructor/enum type membership
-- relations from the original list of definitions. Assumes that there
-- are no duplicates in the input.
createEnumInfo :: [EnumDef] -> EnumInfo
createEnumInfo enums = EnumInfo constrEnum enumConstrs
  where
    constrEnum
      = Map.fromList
      . concatMap (\(EnumDef lname constrs)
                     -> map (\lconstr -> (locVal lconstr, locVal lname)) constrs)
      $ enums

    enumConstrs
      = Map.fromList
      . map (\(EnumDef lname constrs)
               -> (locVal lname, map locVal constrs))
      $ enums

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
  put (TV tv)  = putInt8 0 >> put (encodeUtf8 tv)
  put (TAV tv) = putInt8 1 >> put (encodeUtf8 tv)
  get = do
    tag <- getInt8
    let constr =
          case tag of
            0 -> TV
            1 -> TAV
            n -> fail "Inavlid tag deserialized for TVar"
    constr . decodeUtf8 <$> get

instance IsString EnumConstr where
  fromString = EnumConstr . fromString

instance Serialize EnumConstr where
  put = put . unEnumConstr
  get = EnumConstr <$> get

instance (Serialize a) => Serialize (Located a) where
  put (Located loc x) = put (loc,x)
  get = uncurry Located <$> get

instance Serialize Lit where
instance Serialize EnumDef where
instance Serialize Def where
instance Serialize Arg where
instance Serialize Type where
instance Serialize Pattern where
instance Serialize Match where
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

instance Pretty EnumConstr where
  ppr (EnumConstr ec) = ppr ec

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

    ECase e ms -> token Token.case_ <> parens (ppr e) <+> lbrace
                   <$$> indent 2 (vsep (map (semify . ppr) ms))
                   <$$> rbrace

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

instance Pretty Match where
  ppr (Match (Located _ (PatLit p)) expr)
    = ppr (LConstr p) <+> token Token.rarrow <+> maybeBrace expr
    where
      -- Wrap braces around the expression in case it is a sequence of
      -- statements, otherwise don't.
      maybeBrace e
        = case locVal e of
            ESeq _ _ -> lbrace <+> semify (ppr e) <+> rbrace
            _ -> ppr e

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
    LTimeDelta d   -> ppr d
    LConstr ec     -> text "`" <> ppr ec

instance Pretty Type where
  ppr = \case
    TInt        -> token Token.int
    TFloat      -> token Token.float
    TFixed prec -> ppr prec
    TBool       -> token Token.bool
    TAny        -> token Token.any
    TAssetAny   -> token Token.asset
    TAddress    -> token Token.contract
    TAccount    -> token Token.account
    TAsset TBinary   -> token Token.assetBin
    TAsset TDiscrete -> token Token.assetDis
    TAsset (TFractional Prec1) -> token Token.assetFrac1
    TAsset (TFractional Prec2) -> token Token.assetFrac2
    TAsset (TFractional Prec3) -> token Token.assetFrac3
    TAsset (TFractional Prec4) -> token Token.assetFrac4
    TAsset (TFractional Prec5) -> token Token.assetFrac5
    TAsset (TFractional Prec6) -> token Token.assetFrac6
    TContract   -> token Token.contract
    TVoid       -> token Token.void
    TSig        -> token Token.sig
    TMsg        -> token Token.msg
    TError      -> text $ "<error-type>"  -- Not in syntax
    TVar (TV v) -> text $ toSL v
    TVar (TAV v) -> text $ toSL v
    TRef t      -> hcat ["ref <", ppr t, ">"]  -- Not in syntax
    TCrypto t   -> hsep [ppr Token.local, ppr t]
    TDateTime   -> token Token.datetime
    TTimeDelta  -> token Token.timedelta
    TState      -> token Token.state
    TEnum e     -> token Token.enum <+> token (unName e)

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
    VEnum c      -> ppr c
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

instance Pretty EnumDef where
  ppr (EnumDef lname lconstrs)
    = token Token.enum <+> ppr (locVal lname) <+> lbrace
      <$$> (Pretty.commafy . map (ppr . locVal) $ lconstrs)
      <$$> rbrace <> token Token.semi

instance Pretty Def where
  ppr = \case
    GlobalDefNull typ (Located _ name) -> hsep [token Token.global, ppr typ, ppr name] <> token Token.semi
    LocalDefNull typ (Located _ name)  -> hsep [token Token.local, ppr typ, ppr name] <> token Token.semi
    GlobalDef typ name expr             -> hsep [token Token.global, ppr typ, ppr name `assign` ppr expr]
    LocalDef typ name expr              -> hsep [token Token.local, ppr typ, ppr name `assign` ppr expr]

instance Pretty Script where
  ppr (Script enums defns graph methods) = vsep
    [ vsep (map ppr enums)
    , vsep (map ppr defns)
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
  LConstr c -> VEnum c

evalLLit :: LLit -> Script.Value
evalLLit (Located _ lit) = evalLit lit

-------------------------------------------------------------------------------
-- To/FromJSON
-------------------------------------------------------------------------------

instance ToJSON Name where
  toJSON (Name nm) = toJSON nm

instance FromJSON Name where
  parseJSON = fmap Name . parseJSON

instance ToJSON DateTime where
  toJSON (DateTime dt) = toJSON dt

instance FromJSON DateTime where
  parseJSON = fmap DateTime . parseJSON

-------------------------------------------------------------------------------
-- Postgres Serialization
-------------------------------------------------------------------------------

instance ToField Script where
  toField = EscapeByteA . Data.Serialize.encode

instance FromField Script where
  fromField f mdata = do
    bs <- fromField f mdata
    case Data.Serialize.decode <$> bs of
      Nothing             -> returnError UnexpectedNull f ""
      Just (Left err)     -> returnError ConversionFailed f err
      Just (Right script) -> return script

instance ToField Value where
  toField = EscapeByteA . Data.Serialize.encode

instance FromField Value where
  fromField f mdata = do
    bs <- fromField f mdata
    case Data.Serialize.decode <$> bs of
      Nothing          -> returnError UnexpectedNull f ""
      Just (Left err)  -> returnError ConversionFailed f err
      Just (Right val) -> return val

instance ToField [Value] where
  toField = EscapeByteA . Data.Serialize.encode

instance FromField [Value] where
  fromField f mdata = do
    bs <- fromField f mdata
    case Data.Serialize.decode <$> bs of
      Nothing           -> returnError UnexpectedNull f ""
      Just (Left err)   -> returnError ConversionFailed f err
      Just (Right vals) -> return vals
