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

import Script.Pretty hiding ((<>))
import Contract (InvalidMethodName)
import Script (Name)
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
  | HomomorphicFail Text                -- ^ Error when performing homomorphic ops on SafeInteger
  | InvalidState Label GraphState       -- ^ Invalid graph state entry
  | SubgraphLock                        -- ^ Subraph lock is held
  | LocalVarNotFound Name               -- ^ Variable lookup failed
  | Impossible Text                     -- ^ Internal error
  | HugeInteger Text                    -- ^ SafeInteger bounds exceeded
  | HugeString Text                     -- ^ SafeString bounds exceeded
  | NoSuchPrimOp Name                   -- ^ Prim op name lookup fail
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
    HomomorphicFail err                -> "Homomorphic fail:" <+> ppr err
    InvalidState label graphState      -> "Invalid state:" <+> ppr label <+> ", actual" <+> ppr graphState
    SubgraphLock                       -> "Subgraph lock is held"
    LocalVarNotFound nm                -> "Local var not found:" <+> ppr nm
    Impossible err                     -> "Internal error:" <+> ppr err
    HugeInteger err                    -> "SafeInteger bounds exceeded:" <+> ppr err
    HugeString err                     -> "SafeString bounds exceeded:" <+> ppr err
    NoSuchPrimOp nm                    -> "No such primop:" <+> ppr nm
