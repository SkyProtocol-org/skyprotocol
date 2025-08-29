module Utils where

import Data.Char (toLower)
import Data.Fixed
import Data.Time
import Data.Time.Clock qualified as DTC
import Data.Time.Clock.POSIX
import Data.Time.Clock.POSIX qualified as DTCP
import PlutusLedgerApi.V1.Time qualified as T (POSIXTime (..))

dropPrefix :: String -> String -> String
dropPrefix pr s = case splitAt (length pr) s of
  (p, rest) | p == pr -> toLowerHead rest
  _ -> s
  where
    toLowerHead :: String -> String
    toLowerHead [] = []
    toLowerHead (x : xs) = toLower x : xs

utcTimeEpoch :: DTC.UTCTime
utcTimeEpoch = DTCP.posixSecondsToUTCTime 0

-- Get current time in Plutus-friendly format
currentPOSIXTime :: IO T.POSIXTime
currentPOSIXTime = do
  utcTime <- getCurrentTime
  let (MkFixed nominalDiffTime) = nominalDiffTimeToSeconds $ diffUTCTime utcTime utcTimeEpoch
  return . T.POSIXTime $ nominalDiffTime `div` 1_000_000_000

posixTimeToPlutusTime :: POSIXTime -> T.POSIXTime
posixTimeToPlutusTime t =
  let (MkFixed nominalDiffTime) = nominalDiffTimeToSeconds t
   in T.POSIXTime $ nominalDiffTime `div` 1_000_000_000_000
