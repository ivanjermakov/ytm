{-# LANGUAGE BlockArguments #-}

module Ytm.App.State.Custom where

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
import Control.Concurrent.Async (async)
import Control.Monad (when)
import Control.Monad.IO.Class
import Data.List (nub, sortOn)
import Data.Maybe (fromJust, isJust)
import qualified Data.Ord as O
import qualified Data.Vector as Vec
import Text.Printf (printf)
import Text.Regex.PCRE
import Ytm.Api
import Ytm.App.State.Core
import Ytm.App.Types
import Ytm.FileSystem
import Ytm.Util.Persistence

credentialsLoadedH :: Credentials -> State -> T.EventM ResourceName (T.Next State)
credentialsLoadedH c s = do
  sendChan (Log "credentials loaded" Info) s'
  liftIO . async $ do
    l <- loadFromDump s'
    when (isJust l) do
      sendChan (DumpLoaded $ fromJust l) s'
      sendChan FsChanged s'
  M.continue (s {sCredentials = Just c})
  where
    s' = s {sCredentials = Just c}

settingsLoadedH :: Settings -> State -> T.EventM ResourceName (T.Next State)
settingsLoadedH settings s = do
  sendChan (Log "settings loaded" Info) s'
  liftIO . async $ do
    c <- credentials
    sendChan (CredentialsLoaded c) s'
  M.continue s'
  where
    s' = s {sSettings = Just settings}

-- TODO: treat being downloaded (part) files
fsChangedH :: State -> T.EventM ResourceName (T.Next State)
fsChangedH s = do
  files <- liftIO . listDownloadedFiles . downloadedPath . fromJust . sSettings $ s
  M.continue $ s {sDownloadedFiles = files, sVideosL = nVideosL files}
  where
    mapVi fs i =
      case (isD i, isDing i) of
        (True, False) -> i {itemProgress = Just progressDownloaded, itemStatus = Downloaded}
        (True, True) -> i
        _ -> i {itemProgress = Nothing, itemStatus = Available}
      where
        isD = isDownloaded fs . videoId . itemVideo
        isDing = (== Downloading) . itemStatus
    isDownloaded fs vId = any (`match` vId) fs
    match :: FilePath -> VideoId -> Bool
    match f vId = f =~ (printf "^%s\\..*" vId :: String)
    nVideosL :: [FilePath] -> L.List ResourceName VideoItem
    nVideosL fs = fmap (mapVi fs) . sVideosL $ s

dumpLoadedH :: ([Channel], [Video]) -> State -> T.EventM ResourceName (T.Next State)
dumpLoadedH (chs, vs) s = do
  sendChan VideosLoaded s
  sendChan (Log "videos loaded from cache" Info) s
  M.continue (s {sChannels = chs, sVideos = nub vs})

channelsLoadedH :: [Channel] -> State -> T.EventM ResourceName (T.Next State)
channelsLoadedH chs s = do
  sendChan (Log ("channels loaded: " ++ show (length chs)) Info) s
  M.continue (s {sChannels = chs})

channelVideosLoadedH :: [Video] -> State -> T.EventM ResourceName (T.Next State)
channelVideosLoadedH vs s = do
  sendChan (Log ("loading videos from channels: " ++ countStr) Info) s
  M.continue (s {sVideos = nub $ sVideos s ++ vs, sLoadedChannels = sLoadedChannels s + 1})
  where
    countStr =
      printf
        "%d/%d (%d)"
        (1 + sLoadedChannels s)
        (length (sChannels s))
        (length (sVideos s))

videosLoadedH :: State -> T.EventM ResourceName (T.Next State)
videosLoadedH s = do
  sendChan (Log (printf "loaded %d videos" (length . sVideos $ s)) Info) s
  dumpVs
  w <- videoListWidth s
  sendChan FsChanged s
  M.continue (s {sVideosL = L.list VideoList (Vec.fromList vItems) 1, sVideosLWidth = w})
  where
    vItems = map (\v -> VideoItem v Nothing Available) . sortOn (O.Down . publishedAt) . sVideos $ s
    dumpVs = liftIO $ dump (videosDumpPath . fromJust . sSettings $ s) (sVideos s)

videoDownloadedH :: VideoId -> FilePath -> State -> T.EventM ResourceName (T.Next State)
videoDownloadedH vId vPath s = do
  sendChan (Log ("downloaded video: " ++ vPath) Info) s
  sendChan FsChanged s
  M.continue $
    updateVideoL
      (\i -> i {itemProgress = Just progressDownloaded, itemStatus = Downloaded})
      vId
      s

downloadProgressH :: VideoId -> Progress -> State -> T.EventM ResourceName (T.Next State)
downloadProgressH vId p s = M.continue $ updateVideoL (\i -> i {itemProgress = Just p}) vId s

logH :: String -> LogLevel -> State -> T.EventM ResourceName (T.Next State)
logH m l s = M.continue $ s {sStatus = m, sLog = sLog s ++ [(l, m)]}
