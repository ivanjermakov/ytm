module Ytm.Play where

import System.Process
import Text.Printf (printf)

play :: FilePath -> String -> IO ()
play path pattern = do
  _ <- readCreateProcessWithExitCode (shell . printf pattern $ path) ""
  return ()
