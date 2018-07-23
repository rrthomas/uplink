{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Network.P2P.Logging.Rule
  ( LogRule(..)
  , LogRuleSource(..)
  , LogRuleDestination(..)
  , LogRuleSeverity(..)
  , Severity(..)
  , Colors(..)

  , matchRules
  , verifyRules
  , toggleVerbosity
  ) where

import Protolude

import Data.Binary (Binary)
import Data.List (nubBy)

-------------------------------------------------------------------------------
-- Logging rules and rule configuration
-------------------------------------------------------------------------------

data LogRule
  = LogRule
    { ruleSource :: LogRuleSource
    , ruleSeverity :: LogRuleSeverity
    , ruleDestination :: LogRuleDestination
    }
  deriving (Show, Eq)

data LogRuleSource
  = AnySource
  | UnknownSource
  | SpecificSource Text
  deriving (Show, Eq)

data LogRuleSeverity
  = AnySeverity
  | SpecificSeverity Severity
  deriving (Show, Eq)

data LogRuleDestination
   = Stdout Colors
   | Stderr Colors
   | LogFile FilePath Colors
  deriving (Show, Eq)

-- | Various levels of severity of log messages
data Severity
  = Info
  | Debug
  | Warning
  | Error
  deriving (Generic, Binary, Eq, Show)

data Colors = Colors | NoColors
  deriving (Show, Eq)

matchRules :: Maybe Text -> Severity -> [LogRule] -> [LogRuleDestination]
matchRules source severity
  = mapMaybe (matchRule source severity)

matchRule :: Maybe Text -> Severity -> LogRule -> Maybe LogRuleDestination
matchRule source severity (LogRule ruleSource ruleSeverity dest)
  = if matchSource ruleSource source && matchSeverity ruleSeverity severity
    then Just dest
    else Nothing
  where
    matchSource AnySource _ = True
    matchSource UnknownSource Nothing = True
    matchSource (SpecificSource x) (Just y) = x == y
    matchSource _ _ = False

    matchSeverity AnySeverity _ = True
    matchSeverity (SpecificSeverity x) y = x == y

parseSource :: Text -> Either Text LogRuleSource
parseSource "*" = pure AnySource
parseSource src = pure (SpecificSource src)

parseSeverity :: Text -> Either Text LogRuleSeverity
parseSeverity "*" = pure AnySeverity
parseSeverity "Error" = pure $ SpecificSeverity Error
parseSeverity "Warning" = pure $ SpecificSeverity Warning
parseSeverity "Debug" = pure $ SpecificSeverity Debug
parseSeverity "Info" = pure $ SpecificSeverity Info
parseSeverity other = Left $ "Unknown severity: " <> other

parseDestination :: Text -> [Text] -> Either Text LogRuleDestination
parseDestination dest colors = (case dest of
                                 "stderr" -> Stderr
                                 "stdout" -> Stdout
                                 other -> LogFile (toS other))
                               <$> parseColors colors

parseColors :: [Text] -> Either Text Colors
parseColors ["colors"] = pure Colors
parseColors ["no colors"] = pure NoColors
parseColors _ = Left "parseColors: did not understand it"

verifyRules :: [[Text]] -> Either Text [LogRule]
verifyRules = mapM verifyRule
  where
    verifyRule (source:severity:dest:colors)
      = LogRule
      <$> parseSource source
      <*> parseSeverity severity
      <*> parseDestination dest colors
    verifyRule _ = Left "verifyRules: failed to verify logging rules configuration"

-- | Add a logging rule that outputs everything to stdout, unless
-- there is already such a rule present.
toggleVerbosity :: Bool -> [LogRule] -> [LogRule]
toggleVerbosity wantVerbosity rules
  = if wantVerbosity
    then nubBy ruleEqWithoutColors (logRuleCatchall : rules)
    else rules
  where
    -- If two rules have the source and severity and destination, but only
    -- differ on whether they have colors enabled or not, then we want to
    -- regard them as equal.
    ruleEqWithoutColors :: LogRule -> LogRule -> Bool
    ruleEqWithoutColors (LogRule srcL sevL destL) (LogRule srcR sevR destR)
      = srcL == srcR
        && sevL == sevR
        && case (destL, destR) of
               (Stderr _, Stderr _) -> True
               (Stdout _, Stdout _) -> True
               (LogFile fpL _, LogFile fpR _) -> fpL == fpR
               (_, _) -> False

-------------------------------------------------------------------------------
-- Testing logging rules
-------------------------------------------------------------------------------

logRuleError :: LogRule
logRuleError
  = LogRule
    { ruleSource = AnySource
    , ruleSeverity = SpecificSeverity Error
    , ruleDestination = Stderr Colors
    }

logRuleCatchall :: LogRule
logRuleCatchall
  = LogRule
    { ruleSource = AnySource
    , ruleSeverity = AnySeverity
    , ruleDestination = Stdout Colors
    }

logRuleBlockgenProc :: LogRule
logRuleBlockgenProc
  = LogRule
    { ruleSource = SpecificSource "blockgenproc"
    , ruleSeverity = AnySeverity
    , ruleDestination = LogFile "blockgenproc.log" Colors
    }

defaultRules :: [LogRule]
defaultRules = [logRuleBlockgenProc, logRuleError, logRuleCatchall]
