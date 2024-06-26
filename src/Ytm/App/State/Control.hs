{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module Ytm.App.State.Control where

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (async, mapConcurrently, mapConcurrently_)
import qualified Control.Exception as E
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Either (fromRight)
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
  let ns = (s {sChannels = [], sVideos = [], sLoadedChannels = 0})
      c = fromJust . sCredentials $ ns
  sendChan (Log "refreshing videos" Info) ns
  void . liftIO . forkIO $ do
    chs <- subscriptions c
    dumpChs chs
    sendChan (ChannelsLoaded chs) ns
    mapConcurrently_ (loadChVideos c) chs
    sendChan VideosLoaded ns
  M.continue ns
  where
    dumpChs = dump (channelsDumpPath . fromJust . sSettings $ s)
    loadChVideos c ch = do
      db <- daysBefore (fetchDays . fromJust . sSettings $ s)
      cVs <- fromRight [] <$> (E.try (channelVideos db ch c) :: IO (Either E.SomeException [Video]))
      sendChan (ChannelVideosLoaded cVs) s

downloadVideoH :: State -> T.EventM ResourceName (T.Next State)
downloadVideoH s =
  do
    let vis =
          filter ((== Available) . itemStatus)
            . selectedVideoItems
            $ s
    unless (null vis) do
      sendChan (Log (printf "downloading %d videos" (length vis)) Info) s
    s' <- foldlM d s vis
    M.continue s' {sSelectMode = Nothing}
  where
    d s' i = do
      let vid = videoId . itemVideo $ i
          ns = updateVideoL (\vi -> vi {itemStatus = Downloading}) vid s'
      liftIO $ async do
        mp <- download vid dPath pattern $ logF ns
        case mp of
          Nothing -> return ()
          Just p -> sendChan (VideoDownloaded vid p) ns
      return ns
    logF s' (i, p) = sendChan (DownloadProgress i p) s'
    pattern = downloadCommandPattern . fromJust . sSettings $ s
    dPath = downloadedPath . fromJust . sSettings $ s

deleteDownloadedH :: State -> T.EventM ResourceName (T.Next State)
deleteDownloadedH s = do
  liftIO $ async do
    let ps = mapMaybe (`filePath` s) . filter ((== Downloaded) . itemStatus) . selectedVideoItems $ s
    liftIO . mapConcurrently_ deleteDownloaded $ ps
    unless (null ps) $ sendChan (Log (printf "deleted %d videos" (length ps)) Info) s
    sendChan FsChanged s
  M.continue s {sSelectMode = Nothing}

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
      void . liftIO . forkIO $ play p pattern
    _ -> return ()
  M.continue s {sSelectMode = Nothing}
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
