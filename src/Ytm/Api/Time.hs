module Ytm.Api.Time where

import Data.Time

daysBefore :: Int -> IO UTCTime
daysBefore n = (`UTCTime` 0) . utctDay . addUTCTime (- nominalDay * fromIntegral n) <$> getCurrentTime

readUTCTime :: String -> UTCTime
readUTCTime = parseTimeOrError True defaultTimeLocale utcTimeFormat

showUTCTime :: UTCTime -> String
showUTCTime = formatTime defaultTimeLocale utcTimeFormat

utcTimeFormat :: String
utcTimeFormat = "%FT%TZ"
