module Ytm.Play where

import System.Process

-- TODO: configurable player
play :: FilePath -> IO ()
play p = do
  _ <- readProcessWithExitCode "mpv" [p] ""
  return ()
