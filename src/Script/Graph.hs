{-|

Script graph overlay.

-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Script.Graph (
  -- ** Static specification
  Label(..),
  Transition(..),
  validTransition,

  terminalLabel,
  initialLabel,

  -- ** Runtime state
  GraphState(..),
) where

import Protolude hiding (put, get)

import qualified Control.Monad

import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField

import Script.Pretty
import qualified Script.Token as Token

import Data.Aeson (Value(..), ToJSON(..), FromJSON(..))
import Data.Aeson.Types (typeMismatch)
import Data.Serialize (Serialize, put, get, putWord8, getWord8)
import Data.String (IsString(..))

import qualified Hash

-------------------------------------------------------------------------------
-- Static Specification
-------------------------------------------------------------------------------

newtype Label = Label { unLabel :: Text }
  deriving (Eq, Show, Ord, Generic, NFData, Hash.Hashable, ToJSON)

-- | Represents valid state flows
data Transition
  = Initial                      -- ^ Initial state
  | Step Label                   -- ^ Named state
  | Arrow Transition Transition  -- ^ State transition
  | Terminal                     -- ^ Terminal state
  deriving (Eq, Ord, Show, Generic, NFData, Serialize, Hash.Hashable)

instance Serialize Label where
  put (Label nm) = put (encodeUtf8 nm)
  get = Label . decodeUtf8 <$> get

instance IsString Label where
  fromString = Label . toS

instance Pretty Transition where
  ppr = \case
    Initial   -> token Token.initial
    Step nm   -> ppr nm
    Arrow a b -> token Token.transition <+> ppr a <+> token Token.rarrow <+> ppr b
    Terminal  -> token Token.terminal

instance Pretty Label where
  ppr (Label nm) = ppr nm

validTransition :: Transition -> Bool
validTransition = \case
  Arrow Initial a         -> True
  Arrow a Terminal        -> True
  Arrow a Initial         -> False
  Arrow Terminal a        -> False
  Arrow (Step a) (Step b) -> True

  Initial                 -> False
  Terminal                -> False
  Step _                  -> False
  Arrow _ _               -> False

-------------------------------------------------------------------------------
-- Dynamic Graph State
-------------------------------------------------------------------------------

-- | Runtime contract graph state
data GraphState
  = GraphInitial
  | GraphTerminal
  | GraphLabel Label
  deriving (Eq, Ord, Show, Generic, NFData, Hash.Hashable)

instance Pretty GraphState where
  ppr = \case
    GraphInitial   -> token Token.initial
    GraphTerminal  -> token Token.terminal
    GraphLabel lab -> ppr lab

instance Serialize GraphState where
  put = \case
    GraphInitial   -> putWord8 1
    GraphTerminal  -> putWord8 2
    GraphLabel lab -> putWord8 3 >> put lab
  get = do
    tag <- getWord8
    case tag of
      1 -> pure GraphInitial
      2 -> pure GraphTerminal
      3 -> GraphLabel <$> get
      _ -> Control.Monad.fail "Invalid graph element serialization"

instance ToJSON GraphState where
  toJSON = \case
    GraphInitial         -> let (Label initial) = initialLabel in toJSON initial
    GraphLabel (Label l) -> toJSON l
    GraphTerminal        -> let (Label terminal) = terminalLabel in toJSON terminal

instance FromJSON GraphState where
  parseJSON (String s)
    | (Label $ toS s) == initialLabel  = pure GraphInitial
    | (Label $ toS s) == terminalLabel = pure GraphTerminal
    | otherwise  = pure $ GraphLabel $ Label $ toS s
  parseJSON invalid = typeMismatch "GraphState" invalid


instance ToField GraphState where
  toField = \case
    GraphInitial         -> toField $ Token.initial
    GraphLabel (Label l) -> toField l
    GraphTerminal        -> toField $ Token.terminal

instance FromField GraphState where
  fromField f mdata = do
    mText <- fmap (toS :: ByteString -> Text) <$> fromField f mdata
    case mText of
      Nothing  -> returnError UnexpectedNull f ""
      Just lbl
        | lbl == Token.initial  -> pure $ GraphInitial
        | lbl == Token.terminal -> pure $ GraphTerminal
        | otherwise             -> pure $ GraphLabel $ Label lbl


terminalLabel :: Label
terminalLabel = "terminal"

initialLabel :: Label
initialLabel = "initial"
