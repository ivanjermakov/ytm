module Ytm.Download where

import Control.Monad (void)
import System.Directory (listDirectory)
import System.IO (hGetContents)
import System.Process
import Text.Printf (printf)
import Text.Read (readMaybe)
import Text.Regex.PCRE
import Ytm.Api

-- TODO: configurable downloader
download :: VideoId -> FilePath -> ((VideoId, Maybe Float, String) -> IO ()) -> IO (Maybe FilePath)
download vId p hL = do
  (_, Just stdout, _, _) <- createProcess (shell cmd) {std_out = CreatePipe}
  ms <- hGetContents stdout
  void . mapM (hL . (\s -> (vId, readMaybe (s =~ rx), s))) . drop 1 . lines $ ms
  case lines ms of
    [] -> return Nothing
    l : _ -> return $ Just l
  where
    cmd = printf "yt-dlp %s -o '%%(id)s.%%(ext)s' -P '%s' -O '%%(id)s.%%(ext)s' --progress --newline --no-simulate" vId p
    rx = "(\\d+\\.?\\d*)(?=%)"

listDownloadedFiles :: FilePath -> IO [FilePath]
listDownloadedFiles = listDirectory
