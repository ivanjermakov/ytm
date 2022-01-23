module Main where

import qualified Configuration.Dotenv as C
import Data.List (sortOn)
import qualified Data.Map as M
import qualified Data.Ord as O
import Ytm.Api
import Ytm.Api.Time
import Ytm.Api.VideoRSS

main :: IO ()
main = do
  _ <- C.loadFile C.defaultConfig
  c <- credentials
  let chs =
        [ Channel "UC3tNpTOHsTnkmbwztCs30sA" "Nothernlion",
          Channel "UCidjPBdcX89oul0g13hdDtg" "Fine Design",
          Channel "UC9BvCxNJtIjphlDLQ8MZz3A" "Bulkin",
          Channel "UCgBjLPpPe8ytK0kAncnagdg" "SlivkiShow",
          Channel "UCY03gpyR__MuJtBpoSyIGnw" "Droider.ru",
          Channel "UCT-Dr0SSWbWrCG_xXVpiFew" "Sonchyk"
        ]
  pubB <- daysBefore 2
  print pubB
  vss <- mapM channelVideos chs
  let m =
        M.fromList
          . map (\vs' -> (channel (head vs'), vs'))
          . filter (not . null)
          $ vss
  let vs = sortOn (O.Down . publishedAt). filter (\v -> publishedAt v > pubB) . concatMap snd . M.toList $ m
  putStrLn . unlines . map (\v -> unwords [channelName . channel $ v, show $ publishedAt v, videoTitle v]) $ vs
  --  _ <- runApp
  return ()
