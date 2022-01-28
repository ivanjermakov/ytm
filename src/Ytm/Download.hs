module Ytm.Download where

import Control.Monad (void)
import System.IO (hGetContents)
import System.Process
import Text.Printf (printf)
import Text.Read (readMaybe)
import Text.Regex.PCRE
import Ytm.Api

-- TODO: configure sponsorblock
-- TODO: cancel download (store each pid)
download :: VideoId -> FilePath -> String -> ((VideoId, Maybe Float, String) -> IO ()) -> IO (Maybe FilePath)
download vId path pattern hL = do
  (_, Just stdout, _, _) <- createProcess (shell cmd) {std_out = CreatePipe}
  ms <- hGetContents stdout
  void . mapM (hL . (\s -> (vId, readMaybe (s =~ rx), s))) . drop 1 . lines $ ms
  case lines ms of
    [] -> return Nothing
    l : _ -> return $ Just l
  where
    cmd = printf pattern vId path
    rx = "(\\d+\\.?\\d*)(?=%)"
