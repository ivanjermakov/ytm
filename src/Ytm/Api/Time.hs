module Ytm.Api.Time where

import Data.Time

daysBefore :: Int -> IO UTCTime
daysBefore n = (`UTCTime` 0) . utctDay . addUTCTime (- nominalDay * fromIntegral n) <$> getCurrentTime

readUTCTime :: String -> String -> UTCTime
readUTCTime = parseTimeOrError True defaultTimeLocale

showUTCTime :: String -> UTCTime -> String
showUTCTime = formatTime defaultTimeLocale

utcTimeFormat :: String
utcTimeFormat = "%FT%TZ"

utcTimeFormatWithMillis :: String
utcTimeFormatWithMillis = "%FT%T.000Z"

