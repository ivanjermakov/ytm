module Main where

import qualified Configuration.Dotenv as C
import Ytm.Api
import Ytm.Api.Time
import Ytm.Api.Video

main :: IO ()
main = do
  _ <- C.loadFile C.defaultConfig
  c <- credentials
  pubB <- daysBefore 2
  let chs =
        [ Channel "UC3tNpTOHsTnkmbwztCs30sA" "Nothernlion" Nothing,
          Channel "UCidjPBdcX89oul0g13hdDtg" "Fine Design" Nothing,
          Channel "UC9BvCxNJtIjphlDLQ8MZz3A" "Bulkin" Nothing,
          Channel "UCgBjLPpPe8ytK0kAncnagdg" "SlivkiShow" Nothing,
          Channel "UCY03gpyR__MuJtBpoSyIGnw" "Droider.ru" Nothing,
          Channel "UCT-Dr0SSWbWrCG_xXVpiFew" "Sonchyk" Nothing
        ]
  upIds <- channelsPlaylistId chs c
  print upIds
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
