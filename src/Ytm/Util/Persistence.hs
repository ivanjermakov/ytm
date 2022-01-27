module Ytm.Util.Persistence where

import System.Directory (doesFileExist)
import System.IO.Strict
import Ytm.Api
import Prelude hiding (readFile)

dump :: (Show a) => FilePath -> a -> IO ()
dump p = writeFile p . show

-- TODO: refactor
loadChannels :: FilePath -> IO (Maybe [Channel])
loadChannels p = do
  exist <- doesFileExist p
  if exist
    then do
      inp <- readFile p
      return $ Just (read inp :: [Channel])
    else return Nothing

loadVideos :: FilePath -> IO (Maybe [Video])
loadVideos p = do
  exist <- doesFileExist p
  if exist
    then do
      inp <- readFile p
      return $ Just (read inp :: [Video])
    else return Nothing
