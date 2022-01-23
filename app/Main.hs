module Main where

import qualified Configuration.Dotenv as C
import Ytm.Api (credentials)
import qualified Ytm.Api as A
import Ytm.Api.Time (daysBefore)
import Ytm.Api.VideoRSS (channelVideos)

main :: IO ()
main = do
  _ <- C.loadFile C.defaultConfig
  c <- credentials
  let ch = A.Channel "UC3tNpTOHsTnkmbwztCs30sA" "Nothernlion"
  pubB <- daysBefore 10
  vs <- channelVideos ch
  putStr . unlines . map show $ vs
  --  _ <- runApp
  return ()
