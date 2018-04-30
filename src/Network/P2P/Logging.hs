{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | A logging service
module Network.P2P.Logging
  (
  -- * Logging functions
    info
  , debug
  , warning
  , critical
  -- * Logging process
  , loggerProc
  -- * Logging configuration
  , LogRule(..)
  , verifyRules
  , toggleVerbosity
  ) where

import Protolude

import Control.Distributed.Process.Lifted
import Control.Distributed.Process.Lifted.Class
import Control.Monad (forever, unless, void)

import Data.Binary
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Hourglass as Hourglass
import qualified System.Hourglass as Hourglass (dateCurrent)
import qualified System.Directory as Dir

import qualified Network.P2P.Service as Service

import Utils (safeWrite, safeWithFile)

-------------------------------------------------------------------------------
-- Logging functions
-------------------------------------------------------------------------------

-- | Send an informative message to the logging service
info :: (MonadProcess m) => Text -> m ()
info = liftP . sendLogMsg Info

-- | Send a debugging message to the logging service
debug :: (MonadProcess m) => Text -> m ()
debug = liftP . sendLogMsg Debug

-- | Send a warning message to the logging service
warning :: (MonadProcess m) => Text -> m ()
warning = liftP . sendLogMsg Warning

-- | Send a critical message to the logging service
critical :: (MonadProcess m) => Text -> m ()
critical = liftP . sendLogMsg Error

-------------------------------------------------------------------------------
-- Logging process and messages
-------------------------------------------------------------------------------

-- | Logging process. This is to be spawned and registered as the
-- @Logging@ service in order for it to receive logging messages.
loggerProc :: MonadProcessBase m => [LogRule] -> m ()
loggerProc rules = do
  let fps = [fp | (LogRule _ _ (LogFile fp _)) <- rules]
  liftIO $ mapM_ (\fp -> do
                    fileExists <- Dir.doesFileExist fp
                    unless fileExists $ void $ safeWrite fp ""
                 ) fps
  forever $ liftP . logMessage rules =<< expect

-- | Various levels of severity of log messages
data Severity
  = Info
  | Debug
  | Warning
  | Error
    deriving (Generic, Binary, Eq, Show)

-- | Logging message sent to logging process
data LogMsg
  = LogMsg
    { logSender :: Maybe Text
    , logSeverity :: Severity
    , logMsg :: Text
    } deriving (Generic, Binary)

logMessage :: [LogRule] -> LogMsg -> Process ()
logMessage rules msg
  = liftIO
  . mapM_ (writeLogMessage msg)
  $ matchRules (logSender msg) (logSeverity msg) rules

-- | Write a log message to the destination (stdout/stderr/file) based on the
-- log rule provided. XXX In the case of a file destination and the file does
-- not exist, the logs will be written to stderr.
writeLogMessage :: LogMsg -> LogRuleDestination -> IO ()
writeLogMessage (LogMsg sender severity msg) dest = do
  time <- Hourglass.dateCurrent
  writeDest (fullMsg time) dest
    where
      writeDest msg' = \case
        Stdout color     -> hPutStrLn stdout (applyColors color severity msg')
        Stderr color     -> hPutStrLn stderr (applyColors color severity msg')
        LogFile fp color -> do
          eRes <- safeWithFile fp AppendMode $ \h ->
            hPutStrLn h (applyColors color severity msg')
          case eRes of -- XXX If file does not exist, log to stderr
            Left err ->
              let altMsg = "(Dumping to stderr because:" <> err <> ") " <> msg'
              in hPutStrLn stderr (applyColors color severity altMsg)
            Right r  -> pure ()

      senderTxt = maybe " " (\s -> " (" <> s <> ") ") sender

      fullMsg time = "[" <> showTime time <> "]" <> senderTxt <> msg

      showTime :: Hourglass.DateTime -> Text
      showTime = Text.pack . Hourglass.timePrint fmt
        where
          fmt = [ Hourglass.Format_Year
                , Hourglass.Format_Text '-'
                , Hourglass.Format_Month2
                , Hourglass.Format_Text '-'
                , Hourglass.Format_Day2
                , Hourglass.Format_Text 'T'
                , Hourglass.Format_Hour
                , Hourglass.Format_Text ':'
                , Hourglass.Format_Minute
                , Hourglass.Format_Text ':'
                , Hourglass.Format_Second
                , Hourglass.Format_Text '.'
                , Hourglass.Format_MilliSecond
                , Hourglass.Format_TzHM_Colon
                ]

      applyColors :: Colors -> Severity -> Text -> Text
      applyColors NoColors _ m = m
      applyColors Colors Info m = m
      applyColors Colors Debug m = "\ESC[35m" <> m <> "\ESC[0m"
      applyColors Colors Warning m = "\ESC[33m" <> m <> "\ESC[0m"
      applyColors Colors Error m = "\ESC[31m" <> m <> "\ESC[0m"

sendLogMsg :: Severity -> Text -> Process ()
sendLogMsg severity msg = do
  info <- getProcessInfo =<< getSelfPid

  let sender :: Maybe Text
      sender = case infoRegisteredNames <$> info of
                 Nothing -> Nothing
                 Just [] -> Nothing
                 Just (name:_) -> Just $ Text.pack name

  loggerPid <- whereis (show Service.Logger)
  case loggerPid of
    Just pid -> send pid (LogMsg sender severity msg)
    Nothing -> liftIO $ putStrLn msg

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
                                 other -> LogFile (Text.unpack other))
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
    then List.nubBy ruleEqWithoutColors (logRuleCatchall : rules)
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
