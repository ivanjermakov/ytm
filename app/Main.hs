module Main where

import qualified Configuration.Dotenv as C
import Text.Printf (printf)
import Ytm.App
import Ytm.App.Types

main :: IO ()
main = do
  _ <- C.loadFile C.defaultConfig
  s <- runApp
  _ <- mapM (\(l, m) -> putStrLn $ printf "[%s] %s" (show l) m) . sLog $ s
  print . fmap itemStatus . sVideosL $ s
  return ()
