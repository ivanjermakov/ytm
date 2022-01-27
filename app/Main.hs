module Main where

import qualified Configuration.Dotenv as C
import Control.Monad (void)
import System.Environment (getEnv)
import Text.Printf (printf)
import Ytm.App
import Ytm.App.Types

main :: IO ()
main = do
  loadConfig
  s <- runApp
  _ <- mapM (\(l, m) -> putStrLn $ printf "[%s] %s" (show l) m) . sLog $ s
  return ()

loadConfig :: IO ()
loadConfig = do
  home <- getEnv "HOME"
  void $ C.loadFile (C.defaultConfig {C.configPath = [home ++ "/.config/ytm/.env"]})
