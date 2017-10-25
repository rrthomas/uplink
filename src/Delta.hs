{-|

Ledger deltas , atomic operations on the ledger state as a result of block evaluation.

-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Delta (
  -- ** Delta operations
  Delta(..),
  AssetOp(..),
  LocalOp(..),

  -- ** Printing
  dumpDeltas,
) where

import Protolude hiding ((<>))
import qualified Data.Map as Map
import qualified Data.Set as Set

import Script
import Address
import Storage
import Script.Pretty
import Script.Error (EvalFail(..))
import Script.Graph (GraphState(..))

-------------------------------------------------------------------------------
-- Delta
-------------------------------------------------------------------------------

-- | A delta is a side-effect induced by running a method on a script, that
-- alters some aspect of the ledger world state. When a block is mined, all
-- deltas are computed for all transactions and applied to the end-result
-- world-state of the block.
data Delta
  -- Contract state changes
  = ModifyGlobal Name Value       -- ^ Modify a contract state variable
  | ModifyLocal Name LocalOp      -- ^ Modify a local state variable
  | ModifyAsset AssetOp           -- ^ Modify an asset
  | ModifyState GraphState        -- ^ Transition the contract state

  -- Transactions
  | Atomic Delta Delta       -- ^ Combine two operations as atomic (swap)
  | Terminate ByteString     -- ^ Terminate the contract

  -- Evaluation failures
  | Failure EvalFail
  deriving (Eq, Ord, Show, Generic, NFData)

data AssetOp
  = TransferTo {
      asset    :: Address -- ^ Asset to transfer
    , amount   :: Int64   -- ^ Amount
    , holder   :: Address -- ^ Holder of the asset
    , contract :: Address -- ^ Contract address
   } -- ^ Transfer holdings to contract

  | TransferFrom {
      asset    :: Address -- ^ Asset to transfer
    , amount   :: Int64   -- ^ Amount
    , to       :: Address -- ^ Receipient
    , contract :: Address -- ^ Contract address
  } -- ^ Transfer holdings from contract to account

  | TransferHoldings {
      from   :: Address   -- ^ Sender
    , asset  :: Address   -- ^ Asset to transfer
    , amount :: Int64     -- ^ Amount
    , to     :: Address   -- ^ Receipient
  } -- ^ Transfer holdings from account to account

  | Revert {
      asset   :: Address  -- ^ Asset to transfer-
  } -- ^ Revert holdings to issuer

  deriving (Eq, Ord, Show, Generic, NFData)

data LocalOp
  = Replace Name   -- ^ Overwrite with another local variable
  | Op BinOp Value -- ^ Operate on a local store variable
  deriving (Eq, Ord, Show, Generic, NFData)

-------------------------------------------------------------------------------
-- Printing
-------------------------------------------------------------------------------

-- | Pretty print delta
instance Pretty Delta where
  ppr = \case
    ModifyGlobal nm val -> "global" <+> ppr nm <+> "=" <+> ppr val
    ModifyLocal nm op -> "local" <+> ppr nm
    ModifyState st -> "state" <+> "=" <+> ppr st

    ModifyAsset op -> case op of
      TransferTo asset amt holder contract       ->
        "transferTo" <+> ppr (Address.shortAddr asset)
      TransferFrom asset amt holder contract     ->
        "transferFrom" <+> ppr (Address.shortAddr asset)
      TransferHoldings asset amt holder contract ->
        "transferHoldings" <+> ppr (Address.shortAddr asset)
      Revert asset                               ->
        "revert" <+> ppr (Address.shortAddr asset)

    Atomic a1 a2 -> "atomic" <+> "{" <+> ppr a1 <+> "," <+> ppr a2 <+> "}"
    Terminate msg -> "terminate" <> parens (dquotes (ppr msg))
    Failure mode -> "failure" <> parens (ppr (show mode :: Text))

-- | Pretty print delta list
dumpDeltas :: [Delta] -> Doc
dumpDeltas deltas = indent 8 $ vcat (fmap ppr deltas)
