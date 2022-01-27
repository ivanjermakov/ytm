{-# LANGUAGE TupleSections #-}

module Ytm.Download where

import Control.Monad (void)
import System.IO (hGetContents)
import System.Process
import Text.Printf (printf)
import Ytm.Api

download :: VideoId -> FilePath -> (String -> IO ()) -> IO (Maybe FilePath)
download vId p hL = do
  -- TODO: configurable downloader
  let cmd = printf "yt-dlp %s -o '%%(id)s.%%(ext)s' -P '%s' -O '%%(id)s.%%(ext)s' --progress --newline --no-simulate" vId p
  (_, Just stdout, _, _) <- createProcess (shell cmd) {std_out = CreatePipe}
  ms <- hGetContents stdout
  void . mapM hL . drop 1 . lines $ ms
  case lines ms of
    [] -> return Nothing
    l : _ -> return $ Just l
