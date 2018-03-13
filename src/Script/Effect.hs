{-|

Compute what effects an FCL expression has

-}

{-# LANGUAGE TupleSections #-}

module Script.Effect
  ( Effect(..)
  , ScriptEffects(..)
  , EffectError
  , effectCheckScript
  , effectCheckExpr
  , combineSigsEffects
  ) where

import Protolude

import qualified Data.List as List
import qualified Data.Either as Either

import Script
import Script.Pretty hiding ((<>))
import Script.Prim
import Script.Typecheck (Sig)

-- | Effect lattice
data Effect = NoEffect
            | Read
            | Write
            | ReadWrite
  deriving (Show, Eq)

meet :: Effect -> Effect -> Effect
meet NoEffect r = r
meet Read NoEffect = Read
meet Read Read = Read
meet Read Write = ReadWrite
meet Read ReadWrite = ReadWrite
meet Write NoEffect = Write
meet Write Read = ReadWrite
meet Write Write = Write
meet Write ReadWrite = ReadWrite
meet ReadWrite _ = ReadWrite

data EffectError
  = EffectMismatch { effectExpected :: Effect
                   , effectActual :: Effect
                   , effectLocation :: Loc
                   }
  deriving Show

instance Pretty Effect where
  ppr NoEffect = text "{}"
  ppr Read = text "{read}"
  ppr Write = text "{write}"
  ppr ReadWrite = text "{read, write}"

instance Pretty [EffectError] where
  ppr []
    = "No errors"
  ppr errs@(_:_)
    = vcat . map ppr $ errs

instance Pretty EffectError where
  ppr err = (case effectExpected err of
               NoEffect -> "Expected no effects"
               expected -> "Expected effects:" <+> ppr expected)
            <$$> "Actual effects:" <+> ppr (effectActual err)
            <$$> "location:" <+> ppr (effectLocation err)

data ScriptEffects
  = ScriptEffects
    { globalEffects :: [(Name, Effect)]
    , methodEffects :: [(Name, Effect)]
    }
  deriving Show

combineSigsEffects :: [(Name,Sig)] -> ScriptEffects -> [(Name,Sig,Effect)]
combineSigsEffects sigs effects
  = mapMaybe lookupEffect sigs
  where
    lookupEffect (name, sig)
      = (name,sig,) <$> List.lookup name (methodEffects effects)

effectCheckScript
  :: Script -> Either [EffectError] ScriptEffects
effectCheckScript scr
  = case defErrors of
      [] -> pure $ ScriptEffects defEffects
                                 methodEffects
      errs@(_:_) -> Left errs
  where
    (defErrors, defEffects)
      = Either.partitionEithers
        . map effectCheckDef
        . scriptDefs
        $ scr
    methodEffects
      = map effectCheckMethod . scriptMethods $ scr

expectEffect :: Effect -> LExpr -> Either EffectError Effect
expectEffect expected lexpr
  = if expected == actual
    then pure actual
    else Left $ EffectMismatch
           { effectExpected = expected
           , effectActual = actual
           , effectLocation = located lexpr
           }
  where
    actual = effectCheckExpr lexpr

effectCheckDef
  :: Def -> Either EffectError (Name, Effect)
effectCheckDef (GlobalDef ty n lexpr)
  = (n,) <$> expectEffect NoEffect lexpr
effectCheckDef (GlobalDefNull ty n)
  = pure (locVal n, NoEffect)
effectCheckDef (LocalDef ty n lexpr)
  = (n,) <$> expectEffect NoEffect lexpr
effectCheckDef (LocalDefNull ty n)
  = pure (locVal n, NoEffect)

effectCheckMethod
  :: Method -> (Name, Effect)
effectCheckMethod m = (methodName m, effectCheckExpr . methodBody $ m)

effectCheckExpr
  :: LExpr -> Effect
effectCheckExpr (Located loc (ESeq l r))
  = meet (effectCheckExpr l) (effectCheckExpr r)
effectCheckExpr (Located _ (ELit _))
  = NoEffect
effectCheckExpr (Located _ (EVar v))
  = NoEffect
effectCheckExpr (Located _ (EBinOp _ s0 s1))
  = meet (effectCheckExpr s0) (effectCheckExpr s1)
effectCheckExpr (Located _ (EUnOp _ s))
  = effectCheckExpr s
effectCheckExpr (Located _ (EIf c s0 s1))
  = effectCheckExpr c
    `meet` effectCheckExpr s0
    `meet` effectCheckExpr s1
effectCheckExpr (Located _ (ECase s ms))
  = foldr meet NoEffect . map effectCheckExpr $ s : map matchBody ms
effectCheckExpr (Located loc (EBefore c s))
  = meet (effectCheckExpr c) (effectCheckExpr s)
effectCheckExpr (Located loc (EAfter c s))
  = meet (effectCheckExpr c) (effectCheckExpr s)
effectCheckExpr (Located loc (EBetween c0 c1 s))
  = effectCheckExpr c0
    `meet` effectCheckExpr c1
    `meet` effectCheckExpr s
effectCheckExpr (Located loc (EAssign _ s))
  = effectCheckExpr s
effectCheckExpr (Located _ (ECall primopName ss))
  = fromMaybe NoEffect (primEffect <$> lookupPrim primopName)
effectCheckExpr (Located _ ENoOp)
  = NoEffect

primEffect :: PrimOp -> Effect
primEffect Verify
  = Read
primEffect Sign
  = Read
primEffect Block
  = Read
primEffect Deployer
  = Read
primEffect Sender  
  = Read
primEffect Created 
  = Read
primEffect Address 
  = Read
primEffect Validator
  = Read
primEffect Sha256
  = NoEffect
primEffect AccountExists
  = Read
primEffect AssetExists
  = Read
primEffect ContractExists
  = Read
primEffect Terminate
  = Write
primEffect Now
  = Read
primEffect Transition
  = Write
primEffect CurrentState
  = Read
primEffect TxHash
  = Read
primEffect Bound
  = NoEffect
primEffect ContractValueExists
  = Read
primEffect ContractState
  = Read
primEffect NovationInit 
  = Write
primEffect NovationStop 
  = Write
primEffect IsBusinessDayUK
  = Read
primEffect NextBusinessDayUK
  = Read
primEffect IsBusinessDayNYSE
  = Read
primEffect NextBusinessDayNYSE
  = Read
primEffect Between
  = NoEffect
primEffect Fixed1ToFloat
  = NoEffect
primEffect Fixed2ToFloat
  = NoEffect
primEffect Fixed3ToFloat
  = NoEffect
primEffect Fixed4ToFloat
  = NoEffect
primEffect Fixed5ToFloat
  = NoEffect
primEffect Fixed6ToFloat
  = NoEffect
primEffect FloatToFixed1
  = NoEffect
primEffect FloatToFixed2
  = NoEffect
primEffect FloatToFixed3
  = NoEffect
primEffect FloatToFixed4
  = NoEffect
primEffect FloatToFixed5
  = NoEffect
primEffect FloatToFixed6
  = NoEffect
primEffect ContractValue
  = Read
primEffect (AssetPrimOp HolderBalance)
  = Read
primEffect (AssetPrimOp TransferTo)
  = Write
primEffect (AssetPrimOp TransferFrom)
  = Write
primEffect (AssetPrimOp CirculateSupply)
  = Write
primEffect (AssetPrimOp TransferHoldings)
  = Write
