{-# LANGUAGE TupleSections #-}

{-|

Check whether a script does not have any duplicate definitions.

(Does not really take care of shadowing.)

-}

module Script.Duplicate (
  duplicateCheck,
  DuplicateError(..)
) where

import Protolude hiding ((<>))

import qualified Data.Map as Map

import Script
import Script.Pretty
import Utils (duplicates)

-- | Duplicate variable occurrence can be in either a top level definition or
-- an argument to a method/helper function.
data VarSrc = Defn | MethodArg Name

-- | Ways we can have duplicate definitions of various kinds.
data DuplicateError
  = DuplicateMethod Name
  | DuplicateFunction Name
  | DuplicateConstructor EnumConstr
  | DuplicateEnumDef Name
  | DuplicateVariable (VarSrc, VarSrc) Name
  | DuplicateTransition Transition
  | DuplicateAccessRestriction LExpr

instance Pretty DuplicateError where
  ppr (DuplicateMethod m)
    = "Duplicate method:" <+> ppr m
  ppr (DuplicateFunction f)
    = "Duplicate helper function:" <+> ppr f
  ppr (DuplicateConstructor c)
    = "Duplicate constructor:" <+> ppr c
  ppr (DuplicateEnumDef n)
    = "Duplicate enum:" <+> ppr n
  ppr (DuplicateVariable vsrcs nm)
    = case vsrcs of
        (_, Defn) ->
          "Duplicate variable in top level variable definitions:" <+> ppr nm
        (Defn, MethodArg mnm) ->
          sep ["The top level variable"
              , squotes (ppr nm)
              , "is shadowed by an argument in method"
              , squotes (ppr mnm) <> "."
              ]
        (MethodArg mnm, MethodArg _) ->
          "Duplicate method argument name" <+> squotes (ppr nm) <+> "in method" <+> squotes (ppr mnm) <> "."

  ppr (DuplicateTransition t)
    = "Duplicate transition:" <+> ppr t
  ppr (DuplicateAccessRestriction r)
    = "Duplicate role in method access control:" <+> ppr r

instance Pretty [DuplicateError] where
  ppr errs
    = case errs of
        []
          -> "No duplicates found"
        errs@(_:_)
          -> vsep
             $ "Duplicate variables found:"
             : map ((" >" <+>) . ppr) errs

duplicateCheck :: Script -> Either [DuplicateError] Script
duplicateCheck scr@(Script enums defns transitions methods helpers)
  = case allErrs of
      [] -> Right scr
      errs -> Left errs
    where
      allErrs
        = concat
          [ enumDefErrs
          , enumConstrErrs
          , transErrs
          , defnAndMethodArgErrs defns methods
          , methErrs
          , helperErrs
          , methodAccessErrs
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

      transErrs
        = map DuplicateTransition
          . duplicates
          $ transitions

      methErrs
        = map DuplicateMethod . duplicates . map methodName
          $ methods

      methodAccessErrs
        = concatMap (
            map DuplicateAccessRestriction
            . duplicates
            . (\case RoleAny -> []; RoleAnyOf rs -> rs)
            . methodAccess
            )
          $ methods

      helperErrs
        = map DuplicateFunction
          . duplicates
          . map (locVal . helperName)
          $ helpers

-- | Duplicate variable checks for top level definitions and overlap with method
-- arguments. Currently, shadowing of a top level variable with a method
-- argument name is disallowed.
defnAndMethodArgErrs :: [Def] -> [Method] -> [DuplicateError]
defnAndMethodArgErrs defns methods = concatMap defnAndArgErrs methods
  where
    defnVars          = map ( (,Defn) . defnName) defns
    methArgVars m     = map (\arg -> (locVal (argName arg), MethodArg (methodName m))) (methodArgs m)

    defnAndArgErrs m  =
      snd (foldl checkDup (mempty, mempty) (defnVars ++ methArgVars m))

    checkDup (varMap, dupErrs) (nm, src) =
      case Map.lookup nm varMap of
        Nothing     -> (Map.insert nm src varMap, dupErrs)
        Just dupSrc -> let dupErr = DuplicateVariable (dupSrc, src) nm
                        in (varMap, dupErrs ++ [dupErr])
