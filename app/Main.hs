module Main where

import qualified Configuration.Dotenv as C
import Ytm.Api
import Ytm.App

main :: IO ()
main = do
  _ <- C.loadFile C.defaultConfig
  _ <- runApp
  return ()
