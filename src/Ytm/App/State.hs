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

initState :: BChan CustomEvent -> State
initState ch =
  State
    { sSettings =
        -- TODO: make configurable
        Settings
          { fetchDays = 4,
            videosDumpPath = ".cache/videos.dump",
            channelsDumpPath = ".cache/channels.dump",
            downloadedPath = "/D/video/"
          },
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

-- TODO: check log
handleEvent :: State -> T.BrickEvent ResourceName CustomEvent -> T.EventM ResourceName (T.Next State)
handleEvent s e = case e of
  T.AppEvent cusE -> case cusE of
    (CredentialsLoaded c) -> credentialsLoadedH c s
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
