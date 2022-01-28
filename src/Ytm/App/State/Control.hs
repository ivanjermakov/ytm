module Ytm.App.State.Control where

import qualified Brick.Main as M
import qualified Brick.Types as T
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import Ytm.Api
import Ytm.Api.Channel (subscriptions)
import Ytm.Api.Video (channelVideos)
import Ytm.App.State.Core
import Ytm.App.Types
import Ytm.Download (download)
import Ytm.FileSystem (deleteDownloaded)
import Ytm.Play (play)
import Ytm.Util.Persistence (dump)
import Ytm.Util.Time (daysBefore)

loadVideosH :: State -> T.EventM ResourceName (T.Next State)
loadVideosH s = do
  let ns = (s {sStatus = "refreshing videos"})
      c = fromJust . sCredentials $ s
  async $ do
    chs <- subscriptions c
    dumpChs chs
    sendChan (ChannelsLoaded chs) s
    void $ mapConcurrently (chLoaded c) chs
    sendChan VideosLoaded s
  M.continue ns
  where
    dumpChs = dump (channelsDumpPath . fromJust . sSettings $ s)
    chLoaded c ch = do
      db <- daysBefore (fetchDays . fromJust . sSettings $ s)
      cVs <- channelVideos db ch c
      sendChan (ChannelVideosLoaded cVs) s

downloadVideoH :: State -> T.EventM ResourceName (T.Next State)
downloadVideoH s = case mId of
  Just vId -> do
    let s' = updateVideoL (\i -> i {itemStatus = Downloading}) vId s
    sendChan (Log ("downloading video: " ++ vId) Info) s'
    async $ do
      res <- download vId dPath pattern logF
      case res of
        Nothing -> return ()
        Just vPath -> sendChan (VideoDownloaded vId vPath) s'
    M.continue s'
  _ -> M.continue s
  where
    mId = fmap (videoId . itemVideo) . activeVideoItem $ s
    logF (i, p, m) = sendChan (DownloadProgress i p m) s
    pattern = downloadCommandPattern . fromJust . sSettings $ s
    dPath = downloadedPath . fromJust . sSettings $ s

deleteDownloadedH :: State -> T.EventM ResourceName (T.Next State)
deleteDownloadedH s = do
  mp <- liftIO $ activeFilePath s
  case (mp, mId) of
    (Just p, Just vId) -> do
      sendChan (Log ("video deleted: " ++ vId) Info) s
      async $ deleteDownloaded p
      M.continue $ updateVideoL (\i -> i {itemStatus = Available, itemProgress = Nothing}) vId s
    _ -> do
      M.continue s
  where
    mId = fmap (videoId . itemVideo) . activeVideoItem $ s

playH :: State -> T.EventM ResourceName (T.Next State)
playH s = do
  mp <- liftIO . activeFilePath $ s
  case mp of
    Just p -> do
      sendChan (Log ("playing video from " ++ p) Info) s
      async $ play p pattern
    _ -> return ()
  M.continue s
  where
    pattern = playCommandPattern . fromJust . sSettings $ s

resizeH :: State -> T.EventM ResourceName (T.Next State)
resizeH s = do
  w <- videoListWidth s
  M.continue (s {sVideosLWidth = w})
