{-|

Utility functions.

-}

module Utils (
  toByteList,
  showHex,
  ppShow,
  ppDump,
  ppDumpM,

  putRed,
  putGreen,
  dieRed,

  safeRead,
  safeReadLazy,
  safeWrite,

  waitUntil,
  delayedReplicateM_,

  toInt,
  toInt64,
  toWord16,
  toWord64,

  prompt,
  parsePrompt,
  yesOrNoPrompt,

  -- Error handling
  panicImpossible,
) where

import Protolude
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.List (partition)
import qualified Time
import qualified Numeric as N
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Text.Show.Pretty (ppShow)

import System.Console.Haskeline
import Text.Parsec.Text
import Text.Parsec
import Text.Printf (printf)
import System.Console.ANSI
import System.Directory
import qualified Data.Text as T

showHex :: Word8 -> [Char]
showHex = printf "%02x"

toByteList :: ByteString -> [Word8]
toByteList = BA.unpack

ppShow :: Show a => a -> Text
ppShow s = toS (Text.Show.Pretty.ppShow s)

-- | Pretty printer for showable
ppDump :: Show a => a -> IO ()
ppDump x = putStrLn (ppShow x)

-- | Pretty printer for monadic showable
--
-- > Example: ppDumpM testConfig
ppDumpM :: Show a => IO a -> IO ()
ppDumpM m = m >>= \x -> putStrLn (ppShow x)

-- | Print red text
putRed :: Text -> IO ()
putRed msg = do
  setSGR [SetColor Foreground Vivid Red]
  putStrLn msg
  setSGR []

-- | Print green text
putGreen :: Text -> IO ()
putGreen msg = do
  setSGR [SetColor Foreground Vivid Green]
  putStrLn msg
  setSGR []

-- | Half exit with faillure and message.
dieRed :: Text -> IO a
dieRed msg = do
  putRed msg
  exitFailure

waitUntil :: Time.Timestamp -> IO ()
waitUntil t = do
  t' <- Time.now
  let tdiff = t - t'
  when (tdiff > 0) $ do
    putStrLn $ ("Delaying for " <> show tdiff <> "us" :: Text)
    threadDelay (fromIntegral tdiff)

delayedReplicateM_
  :: MonadIO m
  => Int             -- ^ # of replications
  -> NominalDiffTime -- ^ how long to wait in microseconds
  -> m ()            -- ^ action to perform
  -> m ()
delayedReplicateM_ n delay action
  | n <= 0 = return ()
  | otherwise = do
      startTime <- liftIO Time.now
      go startTime 1 n
  where
    go t0 n count
      | count <= 0 = return ()
      | otherwise = do
          action
          let nextTime = t0 + round (fromIntegral n * delay)
          liftIO $ waitUntil nextTime
          go t0 (n+1) (count-1)

{-# INLINE toInt #-}
toInt :: Integral a => a -> Int
toInt = fromInteger . toInteger

{-# INLINE toInt64 #-}
toInt64 :: Integral a => a -> Int64
toInt64 = fromInteger . toInteger

{-# INLINE toWord16 #-}
toWord16 :: Integral a => a -> Word16
toWord16 = fromInteger . toInteger

{-# INLINE toWord64 #-}
toWord64 :: Integral a => a -> Word64
toWord64 = fromInteger . toInteger

-- | Safely read a files contents, failing in a Either
safeRead :: FilePath -> IO (Either Text ByteString)
safeRead fpath = do
  exists <- doesFileExist fpath
  if exists
    then do
      contents <- BS.readFile fpath
      pure $ Right contents
    else do
      pure $ Left $ "File does not exist: " <> (show fpath)

safeReadLazy :: FilePath -> IO (Either Text BSL.ByteString)
safeReadLazy fpath = do
  exists <- doesFileExist fpath
  if exists
    then do
      contents <- BSL.readFile fpath
      pure $ Right contents
    else do
      pure $ Left $ "File does not exist: " <> (show fpath)

safeWrite :: FilePath -> ByteString -> IO (Either Text ())
safeWrite fpath bs =
    fmap (first show) $ try' $ BS.writeFile fpath bs
  where
    try' :: IO a -> IO (Either SomeException a)
    try' = Protolude.try

-- | Calculate the median of list of sortable elements
median :: (Fractional a, Ord a) => [a] -> Maybe a
median xs
  | n < 1 = Nothing
  | even n = Just ((nth xs (div n 2) + nth xs (div n 2 - 1)) / 2.0)
  | otherwise = Just (nth xs (div n 2))
  where
    n = length xs

nth :: Ord t => [t] -> Int -> t
nth (x:xs) n
  | k == n = x
  | k > n = nth ys n
  | otherwise = nth zs $ n - k - 1
  where
    (ys, zs) = partition (< x) xs
    k = length ys
nth [] _ = panic "Cannot index"

prompt :: Text -> IO Text
prompt msg = runInputT defaultSettings $ do
  minput <- getInputLine (toS msg)
  case minput of
    Nothing -> pure ""
    Just input -> pure (toS input)

yesOrNoPrompt :: Text -> IO Bool
yesOrNoPrompt msg = do
  input <- prompt msg
  case T.unpack $ T.toLower input of
    v | v `elem` ["y", "yes"] -> pure True
    v | v `elem` ["n", "no"] -> pure False
    _ -> yesOrNoPrompt msg

parsePrompt :: MonadIO m => Text -> Parser a -> m a
parsePrompt msg p = do
  str <- liftIO $ Utils.prompt msg
  case parse p "<prompt>" (toS str) of
    (Left err) -> do
      print err
      parsePrompt msg p
    (Right v) -> return v
-------------------------------------------------------------------------------
-- Impossible Errors
-------------------------------------------------------------------------------

panicImpossible :: Maybe Text -> a
panicImpossible mText = panic $
  (<>) "The impossible happened" $
    case mText of
      Nothing -> "!"
      Just txt -> ": " <> txt <> "!"
