module Main where

import qualified Configuration.Dotenv as C
import Ytm.App

main :: IO ()
main = do
  _ <- C.loadFile C.defaultConfig
  fs <- runApp
  print fs
  return ()
