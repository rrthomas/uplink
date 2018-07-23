{-|

Script evaluation errors.

--}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Script.Error (
  EvalFail(..),
) where

import Protolude hiding (Overflow, Underflow, DivideByZero)

import Data.Serialize (Serialize)

import Address
import Script.Pretty hiding ((<>))
import Contract (InvalidMethodName)
import Script (Name, Value, Method, Loc)
import Script.Graph (GraphState(..), Label)

-- | Scripts either run to completion or fail with a named error.
data EvalFail
  = AssetIntegrity Text                 -- ^ Asset does not support operation over it
  | AddressIntegrity Text               -- ^ Address does not exist
  | ContractIntegrity Text              -- ^ Contract does not exist
  | AccountIntegrity Text               -- ^ Account does not exist
  | InvalidMethodName InvalidMethodName -- ^ Name lookup failure
  | TerminalState                       -- ^ Execution is in terminal state.
  | MethodArityError Name Int Int       -- ^ Call a function with the wrong # of args
  | Overflow                            -- ^ Overflow
  | Underflow                           -- ^ Underflow
  | DivideByZero                        -- ^ Division by zero
  | InvalidState Label GraphState       -- ^ Invalid graph state entry
  | SubgraphLock                        -- ^ Subraph lock is held
  | LocalVarNotFound Name               -- ^ Variable lookup failed
  | Impossible Text                     -- ^ Internal error
  | HugeInteger Text                    -- ^ SafeInteger bounds exceeded
  | HugeString Text                     -- ^ SafeString bounds exceeded
  | NoSuchPrimOp Name                   -- ^ Prim op name lookup fail
  | NotAuthorisedError Method (Address AAccount) -- ^ Issuer not authorised to access method
  | LookupFail Text                     -- ^ Foldable/Traversable type lookup fail
  | ModifyFail Text                     -- ^ Map modify fail
  | CallPrimOpFail Loc Value Text       -- ^ CollPrimOp called on non-collection value
  deriving (Eq, Show, Generic, Serialize, NFData)

instance Pretty EvalFail where
  ppr e = case e of
    AssetIntegrity err                 -> "Asset integrity error:" <+> ppr err
    AddressIntegrity err               -> "Address integrity error:" <+> ppr err
    ContractIntegrity err              -> "Contract integrity error:" <+> ppr err
    AccountIntegrity err               -> "Account integrity error:" <+> ppr err
    InvalidMethodName err              -> "Invalid method name:" -- XXX pretty print invalid method name
    TerminalState                      -> "Contract is in terminal state"
    MethodArityError nm expected given -> "Method arity error: expected" <+> ppr expected <+> "args, given " <+> ppr given
    Overflow                           -> "Overflow"
    Underflow                          -> "Underflow"
    DivideByZero                       -> "DivideByZero"
    InvalidState label graphState      -> "Invalid state:" <+> ppr label <+> ", actual" <+> ppr graphState
    SubgraphLock                       -> "Subgraph lock is held"
    LocalVarNotFound nm                -> "Local var not found:" <+> ppr nm
    Impossible err                     -> "Internal error:" <+> ppr err
    HugeInteger err                    -> "SafeInteger bounds exceeded:" <+> ppr err
    HugeString err                     -> "SafeString bounds exceeded:" <+> ppr err
    NoSuchPrimOp nm                    -> "No such primop:" <+> ppr nm
    NotAuthorisedError m r -> "Issuer" <+> ppr r <+> "not authorised to call method" <+> ppr m
    LookupFail k                       -> "Lookup fail with key:" <+> ppr k
    ModifyFail k                       -> "Modify map failure, no value with key:" <+> ppr k
    CallPrimOpFail loc v msg           -> "Evaluation error at" <+> ppr loc
                                    <$$+> "Invalid value" <+> ppr v
                                    <$$+> ppr msg
