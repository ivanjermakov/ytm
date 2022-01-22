module Main where

import qualified Configuration.Dotenv as C
import Ytm.Api (credentials)
import Ytm.Api.Channel (subscriptions)
import Ytm.Api.Time (daysBefore)
import Ytm.Api.Video (channelVideos)

main :: IO ()
main = do
  _ <- C.loadFile C.defaultConfig
  c <- credentials
  ch : _ <- subscriptions c
  print ch
  pubB <- daysBefore 10
  vs <- channelVideos pubB ch c
  putStr . unlines . map show $ vs
  print $ length vs
  --  _ <- runApp
  return ()
