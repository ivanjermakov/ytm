module Ytm.Download where

import Control.Monad (void)
import System.IO (hGetContents)
import System.Process
import Text.Printf (printf)
import Ytm.Api
import Ytm.App.Types

progressTemplate :: String
progressTemplate = "download:%(progress.downloaded_bytes)s %(progress.total_bytes)s %(progress.speed)s %(progress.elapsed)s %(progress.eta)s"

-- TODO: configure sponsorblock
-- TODO: cancel download (store each pid)
-- TODO: video/audio file download separation
download :: VideoId -> FilePath -> String -> ((VideoId, Progress) -> IO ()) -> IO (Maybe FilePath)
download vId path pattern hL = do
  (_, Just stdout, _, _) <- createProcess (shell cmd) {std_out = CreatePipe}
  ms <- hGetContents stdout
  void . mapM (hL . (\s -> (vId, readProgress s))) . drop 1 . lines $ ms
  case lines ms of
    [] -> return Nothing
    l : _ -> return $ Just l
  where
    cmd = printf (pattern ++ " --progress-template '%s'") vId path progressTemplate

readProgress :: String -> Progress
readProgress s =
  Progress
    { status = if dp == 100 then Downloaded else Downloading,
      downPercent = dp,
      downBytes = floor db,
      totalBytes = floor tb,
      speed = maybeNA sp,
      elapsed = maybeNA eld,
      eta = maybeNA eta'
    }
  where
    (dBytes : tBytes : sp : eld : eta' : _) = words s
    db = read dBytes
    tb = read tBytes
    dp = db / tb * 100
    maybeNA str = case str of "NA" -> Nothing; j -> Just $ read j
