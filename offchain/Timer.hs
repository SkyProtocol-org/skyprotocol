module Timer where

import Control.Concurrent
import Data.Time.Clock.POSIX

oneHourInMicroseconds :: Int
oneHourInMicroseconds = 24*60*1000*1000

repeatEvery :: Int -> IO () -> IO ()
repeatEvery intervalInMicrosecond action = loop
  where
    loop = do
      start <- getPOSIXTime
      action
      end <- getPOSIXTime
      let elapsed = round ((end - start) * 1e6) :: Int
          wait    = max 0 (intervalInMicrosecond - elapsed)
      threadDelay wait
      loop
