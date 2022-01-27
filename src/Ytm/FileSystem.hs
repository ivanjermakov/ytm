module Ytm.FileSystem where

import System.Directory (listDirectory, removeFile)

listDownloadedFiles :: FilePath -> IO [FilePath]
listDownloadedFiles = listDirectory

deleteDownloaded :: FilePath -> IO ()
deleteDownloaded = removeFile
