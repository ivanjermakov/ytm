{-# LANGUAGE BlockArguments #-}

module Main where

import qualified Configuration.Dotenv as C
import Control.Exception (displayException)
import Control.Monad (void)
import Data.Yaml
import System.Environment (getEnv)
import Ytm.App
import Ytm.App.Types

main :: IO ()
main = do
  loadEnv
  es <- loadSettings
  void case es of
    Left err -> do
      error . ("(config error) " ++) . displayException $ err
    Right settings -> do
      s <- runApp settings
      putStrLn . unlines . map show . sLog $ s

loadEnv :: IO ()
loadEnv = do
  cp <- configDirPath
  void $ C.loadFile (C.defaultConfig {C.configPath = [cp ++ ".env"]})

-- TODO pass config path as cmd argument
-- TODO variable expansion for config paths
loadSettings :: (FromJSON a) => IO (Either ParseException a)
loadSettings = do
  cp <- configDirPath
  decodeFileEither $ cp ++ "config.yml"

configDirPath :: IO String
configDirPath = do
  home <- getEnv "HOME"
  return $ home ++ "/.config/ytm/"
