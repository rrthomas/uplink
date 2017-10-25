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
import Script (Name)
import Script.Graph (GraphState(..), Label)

-- | Scripts either run to completion or fail with a named error.
data EvalFail
  = AssetIntegrity Text           -- ^ Asset does not support operation over it
  | AddressIntegrity Text         -- ^ Address does not exist
  | ContractIntegrity Text        -- ^ Contract does not exist
  | AccountIntegrity Text         -- ^ Account does not exist
  | NoSuchMethod Name             -- ^ Name lookup failure
  | SignFailure Text              -- ^ Failed to sign message
  | DateFailure Text              -- ^ Datetime failure
  | TerminalState                 -- ^ Execution is in terminal state.
  | MethodArityError Text Int Int -- ^ Call a function with the wrong # of args
  | Overflow                      -- ^ Overflow
  | Underflow                     -- ^ Underflow
  | DivideByZero                  -- ^ Division by zero
  | HomomorphicFail Text          -- ^ Error when performing homomorphic ops on SafeInteger
  | InvalidState Label GraphState -- ^ Invalid graph state entry
  | SubgraphLock                  -- ^ Subraph lock is held
  | LocalVarNotFound Text         -- ^ Variable lookup failed
  | Impossible Text               -- ^ Internal error
  | HugeInteger Text              -- ^ SafeInteger bounds exceeded
  | HugeString Text               -- ^ SafeString bounds exceeded
  deriving (Eq, Ord, Show, Generic, NFData)

errorCode :: EvalFail -> Int
errorCode = \case
  AssetIntegrity {}    -> 0
  AddressIntegrity {}  -> 1
  ContractIntegrity {} -> 2
  AccountIntegrity {}  -> 3
  NoSuchMethod {}      -> 4
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
