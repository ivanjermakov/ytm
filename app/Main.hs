{-# LANGUAGE OverloadedStrings #-}

module Main where

import Api
import qualified Configuration.Dotenv as C

main :: IO ()
main = do
  _ <- C.loadFile C.defaultConfig
  k <- credentials
  ss <- subscriptions Nothing k
  print ss
  print $ length ss
  return ()
