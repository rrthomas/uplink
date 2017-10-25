{-|

Time and datetime types.

-}

module Time (
  -- ** Types
  Date,
  Timestamp,

  -- ** Unix Time
  now,
  validateTimestamp,
  validateTimestamp',

  posixMicroSecsToDatetime,

) where

import Protolude
import Data.Time.Clock
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Hourglass

import Datetime.Types (Datetime, posixToDatetime)

-- | GMT unix timestamp
type Timestamp = Int64

-- | Time now (in microseconds)
now :: IO Timestamp
now = round <$> (* 1000000) <$> getPOSIXTime

validateTimestamp' :: Int64 -> Int64 -> Timestamp -> IO Bool
validateTimestamp' pastThreshold futureThreshold n = do
  ts <- now
  pure $ and
    [ n < (ts + futureThreshold)
    , n > (ts - pastThreshold)
    ]

validateTimestamp :: Timestamp -> IO Bool
validateTimestamp = validateTimestamp' pastThreshold futureThreshold
  where
    pastThreshold   = 86400 * 1000000 -- 1 day (microsecs)
    futureThreshold = 900  * 1000000  -- 15 minutes (microsecs)

-- | Converts a Posix timestamp to a Datetime value
-- Note: This function expects an Int64 Posix timstamp
-- represented in microseconds, NOT seconds.
posixMicroSecsToDatetime :: Int64 -> Datetime
posixMicroSecsToDatetime = posixToDatetime . (`div` 1000000)
