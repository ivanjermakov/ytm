module Main where

import qualified Brick.Main as M
import qualified Brick.Widgets.List as L
import qualified Configuration.Dotenv as C
import qualified Data.Vector as Vec
import Ytm.App (runApp)

data State = State {list :: L.List () String}

main :: IO ()
main = do
  _ <- C.loadFile C.defaultConfig
  --k <- credentials
  --ss <- subscriptions Nothing k
  --print ss
  --print $ length ss
  _ <- runApp
  return ()
