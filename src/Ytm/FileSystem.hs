module Ytm.FileSystem where

import Control.Monad (when)
import System.Directory (doesFileExist, listDirectory, removeFile)

listDownloadedFiles :: FilePath -> IO [FilePath]
listDownloadedFiles = listDirectory

deleteDownloaded :: FilePath -> IO ()
deleteDownloaded p = do
  e <- doesFileExist p
  when e $ removeFile p
