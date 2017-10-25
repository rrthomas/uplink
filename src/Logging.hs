{-|

Logging subsystem.

-}

module Logging (
  -- ** Setup
  configureLogging,
  parseLogLevel,

  -- ** Logging methods
  info,
  debug,
  warning,
  critical,

  -- ** Testing
  testLogging,
) where

import Protolude
import System.Log (Priority(..))
import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler hiding (setLevel)
import System.Log.Handler.Simple
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (takeDirectory)

-------------------------------------------------------------------------------
-- Logger
-------------------------------------------------------------------------------

info, debug, warning, critical :: MonadIO m => Text -> m ()
info = liftIO . infoM mainProgram . toS
debug = liftIO . debugM mainProgram . toS
warning = liftIO . warningM mainProgram . toS
critical = liftIO . criticalM mainProgram . toS

mainProgram :: [Char]
mainProgram = "MAIN"

defaultLoggerFormat :: [Char]
defaultLoggerFormat = "[$time : $prio] $msg"

-- | Configure logging system
configureLogging :: Text -> FilePath -> IO ()
configureLogging lvl logfile = do
  let level = parseLogLevel lvl

  let dir = takeDirectory logfile
  when (dir /= ".") $ do
    createDirectoryIfMissing True dir

  h <- fileHandler logfile level >>=
       (\h -> return $ setFormatter h (simpleLogFormatter defaultLoggerFormat))
  let fmt = simpleLogFormatter "[$time : $loggername : $prio] $msg"
  updateGlobalLogger mainProgram (setLevel level)
  updateGlobalLogger mainProgram (addHandler h)
  return ()

parseLogLevel :: Text -> Priority
parseLogLevel s = maybe INFO identity (readMaybe (toS s))

-------------------------------------------------------------------------------
-- Testing
-------------------------------------------------------------------------------

testLogging :: IO ()
testLogging = forM_ [1..5] $ \i -> infoM mainProgram (show i)
