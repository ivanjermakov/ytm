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
  let ch = Channel "UC_x5XG1OV2P6uZZ5FSM9Ttw" "Nothernlion"
  upId <- channelPlaylistId ch c
  print upId
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
