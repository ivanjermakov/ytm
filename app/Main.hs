module Main where

import qualified Configuration.Dotenv as C
import Ytm.Api
import Ytm.Api.Channel
import Ytm.Api.Time
import Ytm.Api.Video

main :: IO ()
main = do
  _ <- C.loadFile C.defaultConfig
  c <- credentials
  pubB <- daysBefore 2
  vs <- subscriptionsVideos pubB c
  putStrLn
    . unlines
    . map (\v -> unwords [channelName . channel $ v, show $ publishedAt v, videoTitle v])
    $ vs
  --  _ <- runApp
  return ()
