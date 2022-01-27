module Ytm.Play where

import System.Process

play :: FilePath -> IO ()
play p = do
  _ <- readProcessWithExitCode "mpv" [p] ""
  return ()
