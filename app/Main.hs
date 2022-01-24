module Main where

import qualified Configuration.Dotenv as C
import Ytm.Api
import Ytm.Api.Time
import Ytm.Api.Video
import Ytm.Api.Channel

main :: IO ()
main = do
  _ <- C.loadFile C.defaultConfig
  c <- credentials
  pubB <- daysBefore 1
  chs <- subscriptions c
  vs <- subscriptionsVideos pubB c
  putStr . unlines . map (\(Video (Channel _ n _) _ a t) -> unwords [n, show a, t]) $ vs
  {-
  chs <- subscriptions c
  putStr . unlines . map channelId $ chs
  m <- subscriptionsVideos pubB c
  putStrLn
    . unlines
    . map (concatMap (\v -> unwords [channelName . channel $ v, show $ publishedAt v, videoTitle v]) . snd)
    . M.toList
    $ m
  -}
  --  _ <- runApp
  return ()
