{-# LANGUAGE BlockArguments #-}

module Ytm.App.State.Control where

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (foldlM)
import Data.Maybe (fromJust, mapMaybe)
import qualified Graphics.Vty as V
import Text.Printf (printf)
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
  let ns = (s {sChannels = [], sVideos = []})
      c = fromJust . sCredentials $ ns
  sendChan (Log "refreshing videos" Info) ns
  async $ do
    chs <- subscriptions c
    dumpChs chs
    sendChan (ChannelsLoaded chs) ns
    void $ mapConcurrently (chLoaded c) chs
    sendChan VideosLoaded ns
  M.continue ns
  where
    dumpChs = dump (channelsDumpPath . fromJust . sSettings $ s)
    chLoaded c ch = do
      db <- daysBefore (fetchDays . fromJust . sSettings $ s)
      cVs <- channelVideos db ch c
      sendChan (ChannelVideosLoaded cVs) s

-- TODO: don't download videos that are being downloaded
downloadVideoH :: State -> T.EventM ResourceName (T.Next State)
downloadVideoH s = M.continue =<< (foldlM (\acc vId -> f acc vId) s $ ids)
  where
    f ns vId = do
      let s' = updateVideoL (\i -> i {itemStatus = Downloading}) vId ns
      async $ do
        sendChan (Log ("downloading video: " ++ vId) Info) s'
        res <- download vId dPath pattern logF
        case res of
          Nothing -> return ()
          Just vPath -> sendChan (VideoDownloaded vId vPath) s'
      return s'
    ids = fmap (videoId . itemVideo) . selectedVideoItems $ s
    logF (i, p, m) = sendChan (DownloadProgress i p m) s
    pattern = downloadCommandPattern . fromJust . sSettings $ s
    dPath = downloadedPath . fromJust . sSettings $ s

deleteDownloadedH :: State -> T.EventM ResourceName (T.Next State)
deleteDownloadedH s = do
  async do
    let ps = mapMaybe (`filePath` s) . filter ((== Downloaded) . itemStatus) . selectedVideoItems $ s
    liftIO . mapM_ deleteDownloaded $ ps
    when (not . null $ ps) $ sendChan (Log (printf "deleted %d videos" (length ps)) Info) s
    sendChan FsChanged s
  M.continue s

enterSelectModeH :: State -> T.EventM ResourceName (T.Next State)
enterSelectModeH s = do
  sendChan (Log "[selection mode]" Info) s
  let ns = do
        ix <- fmap fst . L.listSelectedElement . sVideosL $ s
        return $ SelectMode {selectStart = ix, selectEnd = ix}
  M.continue $ s {sSelectMode = ns}

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

escH :: State -> T.EventM ResourceName (T.Next State)
escH s = do
  sendChan (Log "" Info) s
  M.continue $ s {sSelectMode = Nothing}

resizeH :: State -> T.EventM ResourceName (T.Next State)
resizeH s = do
  w <- videoListWidth s
  M.continue $ s {sVideosLWidth = w}

listEventH :: (L.List ResourceName VideoItem -> L.List ResourceName VideoItem) -> V.Event -> State -> T.EventM ResourceName (T.Next State)
listEventH f k s = do
  nl <- f <$> L.handleListEvent k (sVideosL s)
  let ns = do
        sm <- sSelectMode $ s
        ix <- fmap fst . L.listSelectedElement $ nl
        return $ sm {selectEnd = ix}
  M.continue $ s {sVideosL = nl, sSelectMode = ns}
