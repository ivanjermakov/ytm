{-# LANGUAGE BlockArguments #-}

module Ytm.App.State where

import Brick.BChan (BChan)
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Ytm.App.State.Control
import Ytm.App.State.Custom
import Ytm.App.Types

defaultSettings :: Settings
defaultSettings =
  Settings
    { fetchDays = 4,
      videosDumpPath = ".cache/ytm/videos.dump",
      channelsDumpPath = ".cache/ytm/channels.dump",
      downloadedPath = ".",
      downloadCommandPattern = "yt-dlp %s -o '%%(id)s.%%(ext)s' -P '%s' -O '%%(id)s.%%(ext)s' --progress --newline --no-simulate",
      playCommandPattern = "mpv %s"
    }

initState :: BChan CustomEvent -> State
initState ch =
  State
    { sSettings = Nothing,
      bChan = ch,
      sStatus = "",
      sCredentials = Nothing,
      sChannels = [],
      sLoadedChannels = 0,
      sVideos = [],
      sVideosL = L.list VideoList (Vec.fromList []) 1,
      sVideosLWidth = 0,
      sDownloadedFiles = [],
      sLog = []
    }

-- TODO: search
-- TODO: command mode
-- TODO: video info modal
handleEvent :: State -> T.BrickEvent ResourceName CustomEvent -> T.EventM ResourceName (T.Next State)
handleEvent s e = case e of
  T.AppEvent cusE -> case cusE of
    (CredentialsLoaded c) -> credentialsLoadedH c s
    (SettingsLoaded settings) -> settingsLoadedH settings s
    FsChanged -> fsChangedH s
    DumpLoaded d -> dumpLoadedH d s
    (ChannelsLoaded chs) -> channelsLoadedH chs s
    (ChannelVideosLoaded vs) -> channelVideosLoadedH vs s
    VideosLoaded -> videosLoadedH s
    VideoDownloaded vId vPath -> videoDownloadedH vId vPath s
    DownloadProgress vId mp m -> downloadProgressH vId mp m s
    (Log m l) -> logH m l s
  T.VtyEvent k -> case k of
    V.EvKey (V.KChar 'q') [] -> M.halt s
    V.EvKey (V.KChar 'i') [] -> listEvent $ L.listMoveBy (-1)
    V.EvKey (V.KChar 'k') [] -> listEvent $ L.listMoveBy 1
    V.EvKey (V.KChar 'g') [] -> listEvent L.listMoveToBeginning
    V.EvKey (V.KChar 'G') [] -> listEvent L.listMoveToEnd
    V.EvKey (V.KChar 'r') [] -> loadVideosH s
    V.EvKey (V.KChar 'd') [] -> downloadVideoH s
    V.EvKey (V.KChar 'x') [] -> deleteDownloadedH s
    V.EvKey V.KEnter [] -> playH s
    V.EvResize _ _ -> resizeH s
    _ -> listEvent id
    where
      listEvent f = M.continue . (\l -> s {sVideosL = l}) . f =<< L.handleListEvent k (sVideosL s)
