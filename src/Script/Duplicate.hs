{-|

Check whether a script does not have any duplicate definitions.

(Does not really take care of shadowing.)

-}

module Script.Duplicate (
  duplicateCheck,
  DuplicateError(..)
) where

import Protolude

import Script
import Script.Pretty
import Utils (duplicates)

-- | Ways we can have duplicate definitions of various kinds.
data DuplicateError
  = DuplicateMethod Name
  | DuplicateConstructor EnumConstr
  | DuplicateEnumDef Name
  | DuplicateVariable Name
  | DuplicateTransition Transition

instance Pretty DuplicateError where
  ppr (DuplicateMethod m)
    = "Duplicate method:" <+> ppr m
  ppr (DuplicateConstructor c)
    = "Duplicate constructor:" <+> ppr c
  ppr (DuplicateEnumDef n)
    = "Duplicate enum:" <+> ppr n
  ppr (DuplicateVariable v)
    = "Duplicate variable:" <+> ppr v
  ppr (DuplicateTransition t)
    = "Duplicate transition:" <+> ppr t

instance Pretty [DuplicateError] where
  ppr errs
    = case errs of
        []
          -> "No duplicates found"
        errs@(_:_)
          -> vsep
             $ "Duplicate definitions found:"
             : map ((" -" <+>) . ppr) errs

duplicateCheck :: Script -> Either [DuplicateError] Script
duplicateCheck scr@(Script enums vars transitions methods)
  = case allErrs of
      [] -> Right scr
      errs@(_:_) -> Left errs
    where
      allErrs
        = concat
          [ enumDefErrs
          , enumConstrErrs
          , varErrs
          , transErrs
          , methErrs
          ]

      enumDefErrs
        = map DuplicateEnumDef
          . duplicates
          . map (locVal . enumName)
          $ enums
      enumConstrErrs
        = map DuplicateConstructor
          . duplicates
          . concatMap (map locVal . enumConstrs)
          $ enums
      varErrs
        = map DuplicateVariable
          . duplicates
          . map variableName
          $ vars
      transErrs
        = map DuplicateTransition
          . duplicates
          $ transitions
      methErrs
        = map DuplicateMethod
          . duplicates
          . map methodName
          $ methods

-- | Extract the variable name from a global/local variable
-- declaration.
variableName :: Def -> Name
variableName (GlobalDef _ n _) = n
variableName (GlobalDefNull _ n) = locVal n
variableName (LocalDef _ n _) = n
variableName (LocalDefNull _ n) = locVal n
