{-|

Compute what effects an FCL expression has

-}

{-# LANGUAGE TupleSections #-}

module Script.Effect
  ( Effects
  , ScriptEffects(..)
  , EffectError
  , effectCheckScript
  , effectCheckExpr
  , combineSigsEffects
  ) where

import Protolude

import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Script
import Script.Pretty hiding ((<>))
import Script.Prim
import Script.Typecheck (Sig)

newtype Effects = Effects { effectSet :: Set Effect }
  deriving (Show, Eq)

-- | Effect lattice, parametrised by a role type
data Effect
  = Write -- ^ write to ledger state
  | Read -- ^  read from ledger state
  | ReadVar Name -- ^ read from contract state
  | WriteVar Name -- ^ write to contract state
  deriving (Show, Eq, Ord)

meet :: Effects -> Effects -> Effects
meet (Effects l) (Effects r) = Effects (Set.union l r)

noEffect :: Effects
noEffect = Effects Set.empty

writeVar :: Name -> Effects
writeVar = Effects . Set.singleton . WriteVar

readVar :: Name -> Effects
readVar = Effects . Set.singleton . ReadVar

readEff :: Effects
readEff = Effects . Set.singleton $ Read

writeEff :: Effects
writeEff = Effects . Set.singleton $ Write

data EffectError
  = LedgerEffectMismatch
    { effectActual :: Effects
    , effectLocation :: Loc
    }
  | AccessViolation
    { accessExpected :: AccessRestriction
    , accessActual :: AccessRestriction
    , violationVar :: Name
    , violationLocation :: Loc
    }
  | HelperEffect
    { helperName     :: Name
    , helperEffects  :: Effects
    , helperLocation :: Loc
    }
  deriving Show

instance Pretty Effects where
  ppr (Effects effs) = listOf . Set.toList $ effs

instance Pretty Effect where
  ppr Read = "read"
  ppr Write = "write"
  ppr (ReadVar name) = "read" <+> ppr name
  ppr (WriteVar name) = "write" <+> ppr name

instance Pretty [EffectError] where
  ppr []
    = "No errors"
  ppr errs@(_:_)
    = vcat . map ppr $ errs

instance Pretty EffectError where
  ppr LedgerEffectMismatch {..}
    = "Expected no ledger effects"
      <$$> "Actual effects:" <+> ppr effectActual
      <$$> "location:" <+> ppr effectLocation
  ppr AccessViolation {..}
    = "Variable access violation:" <+> ppr violationVar <+> "at" <+> ppr violationLocation
      <$$> "Allowed access:" <+> ppr accessExpected
      <$$> "Actual access:" <+> ppr accessActual
  ppr HelperEffect {..}
    = "Helper effect violation:" <+> ppr helperName
      <$$+> "Helper effects:" <+> ppr helperEffects
      <$$+> "location:" <+> ppr helperLocation

data ScriptEffects
  = ScriptEffects
    { globalEffects :: [(Name, Effects)]
    , methodEffects :: [(Name, Effects)]
    }
  deriving Show

combineSigsEffects :: [(Name,Sig)] -> ScriptEffects -> [(Name,Sig,Effects)]
combineSigsEffects sigs effects
  = mapMaybe lookupEffect sigs
  where
    lookupEffect (name, sig)
      = (name,sig,) <$> List.lookup name (methodEffects effects)

effectCheckScript
  :: Script
  -> Either [EffectError] ScriptEffects
effectCheckScript scr
  = case defErrors ++ accessErrors ++ helperErrors of
      [] -> pure $ ScriptEffects defEffects
                                 (map (\(x,_,y) -> (x,y)) methodEffects)
      errs@(_:_) -> Left errs
  where
    globalVarNms = map defnName (scriptDefs scr)

    (defErrors, defEffects)
      = Either.partitionEithers
        . map (effectCheckDef globalVarNms)
        . scriptDefs
        $ scr

    accessEnv = varAccessibility $ scriptDefs scr

    accessErrors
      = flip concatMap methodEffects $ \(methName, restr, effects) ->
          checkAccessibility accessEnv restr effects

    methodEffects
      = map (effectCheckMethod globalVarNms)
        . scriptMethods
        $ scr

    -- TODO why is this not included in the errors list?
    roleErrors
      = concatMap (effectCheckRole globalVarNms)
        . scriptMethods
        $ scr

    helperErrors
      = mapMaybe (effectCheckHelper globalVarNms)
        . scriptHelpers
        $ scr

effectCheckRole :: [Name] -> Method -> [EffectError]
effectCheckRole gvarNms m =
    lefts $ pure $
      expectEffects gvarNms notAllowed (methodBody m)
  where
    notAllowed (ReadVar _) = False
    notAllowed _ = True

expectNoLedgerEffects :: [Name] -> LExpr -> Either EffectError Effects
expectNoLedgerEffects = flip expectEffects notAllowed
  where
    notAllowed Read = True
    notAllowed Write = True
    notAllowed _ = False

expectEffects :: [Name] -> (Effect -> Bool) -> LExpr -> Either EffectError Effects
expectEffects gVarNms effectPred lexpr
  = if hasOnlyAllowedEffects actual
    then pure actual
    else Left $ LedgerEffectMismatch
           { effectActual = actual
           , effectLocation = located lexpr
           }
  where
    actual = effectCheckExpr gVarNms lexpr

    hasOnlyAllowedEffects = Set.null . Set.filter effectPred . effectSet

effectCheckDef
  :: [Name] -> Def -> Either EffectError (Name, Effects)
effectCheckDef gnms (GlobalDef ty _ n lexpr)
  = (n,) <$> expectNoLedgerEffects gnms lexpr
effectCheckDef gnms (GlobalDefNull ty _ n)
  = pure (locVal n, noEffect)
effectCheckDef gnms (LocalDef ty n lexpr)
  = (n,) <$> expectNoLedgerEffects gnms lexpr
effectCheckDef gnms (LocalDefNull ty n)
  = pure (locVal n, noEffect)

-- | Ordering on restrictions: @subsumes a b@ means that if we satisfy
-- restriction a, we satisfy restriction b.
subsumes :: AccessRestriction -> AccessRestriction -> Bool
subsumes _ RoleAny = True
subsumes RoleAny (RoleAnyOf _) = False
subsumes (RoleAnyOf as) (RoleAnyOf bs) = all (`elem` bs) as

-- | Check given globals and their accessibility, whether the set of
-- effects is allowed given our current restriction.
checkAccessibility
  :: Map Name AccessRestriction -- ^ mapping of globals to their accessibility
  -> AccessRestriction -- ^ current access restriction
  -> Effects -- ^ effect set we are checking against our restriction
  -> [EffectError]
checkAccessibility accessEnv currentAccess
  = mapMaybe (checkVar <=< accessVar) . Set.toList . effectSet
  where
    accessVar :: Effect -> Maybe Name
    accessVar (WriteVar n) = pure n
    accessVar (ReadVar n) = pure n
    accessVar _ = Nothing

    checkVar :: Name -> Maybe EffectError
    checkVar v
      = case Map.lookup v accessEnv of
          Nothing
            -> Nothing
          Just actual
            -> if not (subsumes currentAccess actual)
               then pure
                    $ AccessViolation
                      { accessExpected = actual
                      , accessActual = currentAccess
                      , violationVar = v
                      , violationLocation = NoLoc -- XXX have loc info
                      }
               else Nothing

-- | Create mapping from global name to its access restriction
varAccessibility :: [Def] -> Map Name AccessRestriction
varAccessibility = Map.fromList . map defToAssoc
  where
    defToAssoc :: Def -> (Name, AccessRestriction)
    defToAssoc (GlobalDef _ rs n _) = (n, rs)
    defToAssoc (GlobalDefNull _ rs n) = (locVal n, rs)
    defToAssoc (LocalDef _ n _) = (n, RoleAny)
    defToAssoc (LocalDefNull _ n) = (locVal n, RoleAny)

effectCheckMethod
  :: [Name] -> Method -> (Name, AccessRestriction, Effects)
effectCheckMethod gnms m
  = ( methodName m
    , methodAccess m
    , effectCheckExpr gnms . methodBody $ m
    )

-- | Should have no Effects. In the future, we may specific types of effects,
-- like global variable updates, reads from the ledger, or writes to the ledger.
effectCheckHelper
  :: [Name] -> Helper -> Maybe EffectError
effectCheckHelper gnms (Helper nm _ body)
  | Set.null (effectSet effs) = Nothing
  | otherwise     = Just (HelperEffect (locVal nm) effs (located body))
  where
    effs = effectCheckExpr gnms body

effectCheckExpr
  :: [Name] -> LExpr -> Effects
effectCheckExpr gnms (Located loc (ESeq l r))
  = meet (effectCheckExpr gnms l) (effectCheckExpr gnms r)
effectCheckExpr gnms (Located _ (ELit _))
  = noEffect
-- | If a variable is a global variable, a use of it constitutes a "read" effect
effectCheckExpr gnms (Located _ (EVar v))
  | locVal v `elem` gnms = readVar $ locVal v
  | otherwise = noEffect
effectCheckExpr gnms (Located _ (EBinOp _ s0 s1))
  = meet (effectCheckExpr gnms s0) (effectCheckExpr gnms s1)
effectCheckExpr gnms (Located _ (EUnOp _ s))
  = effectCheckExpr gnms s
effectCheckExpr gnms (Located _ (EIf c s0 s1))
  = effectCheckExpr gnms c
    `meet` effectCheckExpr gnms s0
    `meet` effectCheckExpr gnms s1
effectCheckExpr gnms (Located _ (ECase s ms))
  = foldr meet noEffect . map (effectCheckExpr gnms) $ s : map matchBody ms
effectCheckExpr gnms (Located loc (EBefore c s))
  = meet (effectCheckExpr gnms c) (effectCheckExpr gnms s)
effectCheckExpr gnms (Located loc (EAfter c s))
  = meet (effectCheckExpr gnms c) (effectCheckExpr gnms s)
effectCheckExpr gnms (Located loc (EBetween c0 c1 s))
  = effectCheckExpr gnms c0
    `meet` effectCheckExpr gnms c1
    `meet` effectCheckExpr gnms s
effectCheckExpr gnms (Located loc (EAssign name s))
  | name `elem` gnms = writeVar name `meet` effectCheckExpr gnms s
  | otherwise = effectCheckExpr gnms s
effectCheckExpr gnms (Located _ (ECall (Left primOp) ss))
  = primEffect primOp
effectCheckExpr gnms (Located _ (ECall (Right _) body))
  = noEffect -- Currently, Helper functions are not allowed to have effects
effectCheckExpr gnms (Located _ ENoOp)
  = noEffect

primEffect :: PrimOp -> Effects
primEffect Verify
  = readEff
primEffect Sign
  = readEff
primEffect Block
  = readEff
primEffect Deployer
  = readEff
primEffect Sender
  = readEff
primEffect Created
  = readEff
primEffect Address
  = readEff
primEffect Validator
  = readEff
primEffect Sha256
  = noEffect
primEffect AccountExists
  = readEff
primEffect AssetExists
  = readEff
primEffect ContractExists
  = readEff
primEffect Terminate
  = writeEff
primEffect Now
  = readEff
primEffect Transition
  = writeEff
primEffect CurrentState
  = readEff
primEffect TxHash
  = readEff
primEffect Bound
  = noEffect
primEffect ContractValueExists
  = readEff
primEffect ContractState
  = readEff
primEffect NovationInit
  = writeEff
primEffect NovationStop
  = writeEff
primEffect IsBusinessDayUK
  = readEff
primEffect NextBusinessDayUK
  = readEff
primEffect IsBusinessDayNYSE
  = readEff
primEffect NextBusinessDayNYSE
  = readEff
primEffect Between
  = noEffect
primEffect TimeDiff
  = noEffect
primEffect Fixed1ToFloat
  = noEffect
primEffect Fixed2ToFloat
  = noEffect
primEffect Fixed3ToFloat
  = noEffect
primEffect Fixed4ToFloat
  = noEffect
primEffect Fixed5ToFloat
  = noEffect
primEffect Fixed6ToFloat
  = noEffect
primEffect FloatToFixed1
  = noEffect
primEffect FloatToFixed2
  = noEffect
primEffect FloatToFixed3
  = noEffect
primEffect FloatToFixed4
  = noEffect
primEffect FloatToFixed5
  = noEffect
primEffect FloatToFixed6
  = noEffect
primEffect ContractValue
  = readEff
primEffect (AssetPrimOp HolderBalance)
  = readEff
primEffect (AssetPrimOp TransferTo)
  = writeEff
primEffect (AssetPrimOp TransferFrom)
  = writeEff
primEffect (AssetPrimOp CirculateSupply)
  = writeEff
primEffect (AssetPrimOp TransferHoldings)
  = writeEff
primEffect (MapPrimOp MapInsert)
  = noEffect
primEffect (MapPrimOp MapDelete)
  = noEffect
primEffect (MapPrimOp MapLookup)
  = noEffect
primEffect (MapPrimOp MapModify)
  = noEffect
primEffect (SetPrimOp SetInsert)
  = noEffect
primEffect (SetPrimOp SetDelete)
  = noEffect
primEffect (CollPrimOp Aggregate)
  = noEffect
primEffect (CollPrimOp Transform)
  = noEffect
primEffect (CollPrimOp Filter)
  = noEffect
primEffect (CollPrimOp Element)
  = noEffect
primEffect (CollPrimOp IsEmpty)
  = noEffect
