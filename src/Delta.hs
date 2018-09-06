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

import Script
import Address (Address, AAccount, AAsset, AContract, shortAddr)
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
  deriving (Eq, Show, Generic, NFData)

data AssetOp
  = TransferTo {
      asset    :: Address AAsset    -- ^ Asset to transfer
    , amount   :: Int64             -- ^ Amount
    , holder   :: Address AAccount  -- ^ Holder of the asset
    , contract :: Address AContract -- ^ Contract address
   } -- ^ Transfer holdings to contract

  | TransferFrom {
      asset    :: Address AAsset    -- ^ Asset to transfer
    , amount   :: Int64             -- ^ Amount
    , to       :: Address AAccount  -- ^ Recipient
    , contract :: Address AContract -- ^ Contract address
  } -- ^ Transfer holdings from contract to account

  | TransferHoldings {
      from   :: Address AAccount  -- ^ Sender
    , asset  :: Address AAsset    -- ^ Asset to transfer
    , amount :: Int64             -- ^ Amount
    , to     :: Address AAccount  -- ^ Recipient
  } -- ^ Transfer holdings from account to account

  | Revert {
      asset   :: Address AAsset  -- ^ Asset to transfer-
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
        "transferTo" <+> ppr (shortAddr asset)
      TransferFrom asset amt holder contract     ->
        "transferFrom" <+> ppr (shortAddr asset)
      TransferHoldings asset amt holder contract ->
        "transferHoldings" <+> ppr (shortAddr asset)
      Revert asset                               ->
        "revert" <+> ppr (shortAddr asset)

    Atomic a1 a2 -> "atomic" <+> "{" <+> ppr a1 <+> "," <+> ppr a2 <+> "}"
    Terminate msg -> "terminate" <> parens (dquotes (ppr msg))
    Failure mode -> "failure" <> parens (ppr (show mode :: Text))

-- | Pretty print delta list
dumpDeltas :: [Delta] -> Doc
dumpDeltas deltas = indent 8 $ vcat (fmap ppr deltas)
