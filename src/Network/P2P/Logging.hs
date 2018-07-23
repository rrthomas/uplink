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
  , Logger(..)
  , loggerProc

  -- * Logging configuration
  , LogRule(..)
  , LogRuleDestination(..)
  , Colors(..)
  , verifyRules
  , toggleVerbosity

  , LogMsg(..)
  , sendLogMsg
  ) where

import Protolude

import Control.Distributed.Process.Lifted
import Control.Distributed.Process.Lifted.Class
import Control.Monad (forever, unless, void)

import Data.Binary
import qualified Data.Text as Text
import qualified Data.Hourglass as Hourglass
import qualified System.Hourglass as Hourglass (dateCurrent)
import qualified System.Directory as Dir

import Network.P2P.Logging.Rule

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

{-
   The Logger process is not supervised, because the supervisor
   services (processes) need to send their log messages somewhere. If the
   logging process was supervised, then this would create a cyclic dependency
   as the Logging process would need to be defined as a an Uplink service, but
   the uplink supervisor services would need to call logging functions.

   Note: In the future, we could implement this cyclic dependency by factoring
   out the logging message functions 'info', 'debug', 'warning', 'critical',
   and 'sendLogMsg' into their own module and import that module into
   Network.P2P.Service, but for now the logging process will be the first
   process to be started, and remain unsupervised.
-}


data Logger = Logger
  deriving (Show, Generic, Binary)

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

  loggerPid <- whereis (show Logger)
  case loggerPid of
    Just pid -> send pid (LogMsg sender severity msg)
    Nothing -> liftIO $ putStrLn msg
