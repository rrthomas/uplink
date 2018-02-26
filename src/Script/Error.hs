{-|

Script evaluation errors.

--}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Script.Error (
  EvalFail(..),
  errorCode,
) where

import Protolude hiding (Overflow, Underflow, DivideByZero)

import Data.Serialize (Serialize)

import Contract (InvalidMethodName)
import Script (Name)
import Script.Graph (GraphState(..), Label)

-- | Scripts either run to completion or fail with a named error.
data EvalFail
  = AssetIntegrity ByteString           -- ^ Asset does not support operation over it
  | AddressIntegrity ByteString         -- ^ Address does not exist
  | ContractIntegrity ByteString        -- ^ Contract does not exist
  | AccountIntegrity ByteString         -- ^ Account does not exist
  | InvalidMethodName InvalidMethodName -- ^ Name lookup failure
  | SignFailure ByteString              -- ^ Failed to sign message
  | DateFailure ByteString              -- ^ Datetime failure
  | TerminalState                       -- ^ Execution is in terminal state.
  | MethodArityError Name Int Int       -- ^ Call a function with the wrong # of args
  | Overflow                            -- ^ Overflow
  | Underflow                           -- ^ Underflow
  | DivideByZero                        -- ^ Division by zero
  | HomomorphicFail ByteString          -- ^ Error when performing homomorphic ops on SafeInteger
  | InvalidState Label GraphState       -- ^ Invalid graph state entry
  | SubgraphLock                        -- ^ Subraph lock is held
  | LocalVarNotFound Name               -- ^ Variable lookup failed
  | Impossible ByteString               -- ^ Internal error
  | HugeInteger ByteString              -- ^ SafeInteger bounds exceeded
  | HugeString ByteString               -- ^ SafeString bounds exceeded
  | NoSuchPrimOp Name                   -- ^ Prim op name lookup fail
  deriving (Eq, Show, Generic, Serialize, NFData)



errorCode :: EvalFail -> Int
errorCode = \case
  AssetIntegrity {}    -> 0
  AddressIntegrity {}  -> 1
  ContractIntegrity {} -> 2
  AccountIntegrity {}  -> 3
  InvalidMethodName {} -> 4
  SignFailure {}       -> 5
  DateFailure {}       -> 6
  TerminalState {}     -> 7
  MethodArityError {}  -> 8
  Overflow {}          -> 9
  Underflow {}         -> 10
  DivideByZero {}      -> 11
  HomomorphicFail {}   -> 12
  InvalidState {}      -> 13
  SubgraphLock {}      -> 14
  LocalVarNotFound {}  -> 15
  Impossible {}        -> 16
  HugeInteger {}       -> 17
  HugeString {}        -> 18
  NoSuchPrimOp {}      -> 19
