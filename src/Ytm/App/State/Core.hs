{-# LANGUAGE FlexibleContexts #-}

module Ytm.App.State.Core where

import Brick.BChan (writeBChan)
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.List (find)
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import qualified Data.Vector as Vec
import Text.Printf (printf)
import Text.Regex.PCRE
import Ytm.Api
import Ytm.App.Types
import Ytm.Util.Persistence
import Ytm.Util.Range

videoListWidth :: State -> T.EventM ResourceName Int
videoListWidth s = do
  e <- M.lookupExtent VideoList
  case e of
    Nothing -> return . sVideosLWidth $ s
    Just (T.Extent _ _ (width, _)) -> return width

loadFromDump :: State -> IO (Maybe ([Channel], [Video]))
loadFromDump s = do
  mChs <- loadChannels (channelsDumpPath . fromJust . sSettings $ s)
  mVs <- loadVideos (videosDumpPath . fromJust . sSettings $ s)
  case (mChs, mVs) of
    (Just chs, Just vs) -> return $ Just (chs, vs)
    _ -> return Nothing

sendChan :: (MonadIO m) => CustomEvent -> State -> m ()
sendChan e s = liftIO $ writeBChan (bChan s) e

async :: (MonadIO m) => IO () -> m ()
async = void . liftIO . forkIO

updateVideoL :: (VideoItem -> VideoItem) -> VideoId -> State -> State
updateVideoL f vId s =
  s
    { sVideosL =
        fmap (\i -> if (== vId) . videoId . itemVideo $ i then f i else i)
          . sVideosL
          $ s
    }

activeVideoItem :: State -> Maybe VideoItem
activeVideoItem = fmap snd . L.listSelectedElement . sVideosL

selectedVideoItems :: State -> [VideoItem]
selectedVideoItems s = case sSelectMode s of
  Nothing -> catMaybes . (: []) . fmap snd . L.listSelectedElement . sVideosL $ s
  Just sm -> map snd . filter (\(i, _) -> inRange i sm) . zip [0 :: Int ..] . Vec.toList . L.listElements . sVideosL $ s

selectedFilePaths :: State -> IO [FilePath]
selectedFilePaths s = return . mapMaybe (`filePath` s) $ vis
  where
    vis = selectedVideoItems s

filePath :: VideoItem -> State -> Maybe FilePath
filePath vi s = fmap (dPath ++) . findFilename (videoId . itemVideo $ vi) . sDownloadedFiles $ s
  where
    findFilename :: VideoId -> [FilePath] -> Maybe FilePath
    findFilename vId files = find (=~ (printf "^%s\\..*" vId :: String)) files
    dPath = downloadedPath . fromJust . sSettings $ s

activeFilePath :: State -> IO (Maybe FilePath)
activeFilePath s = case (mId, mSt) of
  (Just vId, Just Downloaded) -> do
    let dPath = downloadedPath . fromJust . sSettings $ s
    return . fmap (dPath ++) . findFilename vId . sDownloadedFiles $ s
  _ -> do
    return Nothing
  where
    mId = fmap (videoId . itemVideo) . activeVideoItem $ s
    mSt = fmap itemStatus . activeVideoItem $ s
    findFilename :: VideoId -> [FilePath] -> Maybe FilePath
    findFilename vId files = find (=~ (printf "^%s\\..*" vId :: String)) files
