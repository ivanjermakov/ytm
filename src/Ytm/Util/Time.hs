module Ytm.Util.Time where

import Data.Time

daysBefore :: Int -> IO UTCTime
daysBefore n = (`UTCTime` 0) . utctDay . addUTCTime (- nominalDay * fromIntegral n) <$> getCurrentTime

showUTCTime :: (FormatTime t) => String -> t -> String
showUTCTime = formatTime defaultTimeLocale
