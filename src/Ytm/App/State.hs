{-# LANGUAGE BlockArguments #-}

module Ytm.App.State where

import Brick.BChan (BChan, writeBChan)
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (void, when)
import Control.Monad.IO.Class
import Data.List (nub, sortOn)
import Data.Maybe (fromJust, isJust)
import qualified Data.Ord as O
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Text.Printf (printf)
import Ytm.Api
import Ytm.Api.Channel (subscriptions)
import Ytm.Api.Time (daysBefore)
import Ytm.Api.Video (channelVideos)
import Ytm.App.Types
import Ytm.Download
import Ytm.Util.Persistence

-- TODO: make configurable
videoDestinationPath :: String
videoDestinationPath = "/D/video/"

initState :: BChan CustomEvent -> State
initState ch =
  State
    { sSettings =
        Settings
          { fetchDays = 4,
            videosDumpPath = ".cache/videos.dump",
            channelsDumpPath = ".cache/channels.dump"
          },
      bChan = ch,
      sStatus = "",
      sCredentials = Nothing,
      sChannels = [],
      sLoadedChannels = 0,
      sVideos = [],
      sVideosL = L.list VideoList (Vec.fromList []) 1,
      sVideosLWidth = 0,
      sLog = []
    }

-- TODO: check log
handleEvent :: State -> T.BrickEvent ResourceName CustomEvent -> T.EventM ResourceName (T.Next State)
handleEvent s e = case e of
  T.AppEvent cusE -> case cusE of
    (CredentialsLoaded c) -> credentialsLoadedH c s
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
    V.EvResize _ _ -> resizeH s
    _ -> listEventH
    where
      listEvent f = M.continue . (\l -> s {sVideosL = l}) . f =<< L.handleListEvent k (sVideosL s)
      listEventH = M.continue . (\l -> s {sVideosL = l}) =<< L.handleListEvent k (sVideosL s)

credentialsLoadedH :: Credentials -> State -> T.EventM ResourceName (T.Next State)
credentialsLoadedH c s = do
  sendChan (Log "credentials loaded" Info) s
  async $ do
    l <- loadFromDump s
    when (isJust l) $ sendChan (DumpLoaded $ fromJust l) s
  M.continue (s {sCredentials = Just c})

dumpLoadedH :: ([Channel], [Video]) -> State -> T.EventM ResourceName (T.Next State)
dumpLoadedH (chs, vs) s = do
  sendChan VideosLoaded s
  M.continue
    ( s
        { sChannels = chs,
          sVideos = vs,
          sStatus = "videos loaded from cache DATE"
        }
    )

channelsLoadedH :: [Channel] -> State -> T.EventM ResourceName (T.Next State)
channelsLoadedH chs s =
  M.continue
    ( s
        { sChannels = chs,
          sStatus = "channels loaded: " ++ show (length chs)
        }
    )

channelVideosLoadedH :: [Video] -> State -> T.EventM ResourceName (T.Next State)
channelVideosLoadedH vs s =
  M.continue
    ( s
        { sVideos = nub $ sVideos s ++ vs,
          sLoadedChannels = sLoadedChannels s + 1,
          sStatus = "loading videos from channels: " ++ countStr
        }
    )
  where
    countStr =
      printf
        "%d/%d (%d)"
        (1 + sLoadedChannels s)
        (length (sChannels s))
        (length (sVideos s))

videosLoadedH :: State -> T.EventM ResourceName (T.Next State)
videosLoadedH s = do
  dumpVs
  w <- videoListWidth s
  M.continue (s {sVideosL = L.list VideoList (Vec.fromList vItems) 1, sVideosLWidth = w})
  where
    vItems = map (\v -> VideoItem v Nothing Available) . sortOn (O.Down . publishedAt) . sVideos $ s
    dumpVs = liftIO $ dump (videosDumpPath . sSettings $ s) (sVideos s)

videoDownloadedH :: VideoId -> FilePath -> State -> T.EventM ResourceName (T.Next State)
videoDownloadedH vId vPath s = do
  sendChan (Log ("downloaded video: " ++ vPath) Info) s
  M.continue $ updateVideoL (\i -> i {itemProgress = Just 100, itemStatus = Downloaded}) vId s

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
    dumpChs = dump (channelsDumpPath . sSettings $ s)
    chLoaded c ch = do
      db <- daysBefore (fetchDays . sSettings $ s)
      cVs <- channelVideos db ch c
      sendChan (ChannelVideosLoaded cVs) s

downloadVideoH :: State -> T.EventM ResourceName (T.Next State)
downloadVideoH s = case mId of
  Nothing -> do
    sendChan (Log "no selected video to download" Info) s
    M.continue s
  Just vId -> do
    let s' = updateVideoL (\i -> i {itemStatus = Downloading}) vId s
    sendChan (Log ("downloading video: " ++ vId) Info) s'
    async $ do
      res <- download vId videoDestinationPath logF
      case res of
        Nothing -> return ()
        Just vPath -> sendChan (VideoDownloaded vId vPath) s'
    M.continue s'
  where
    mId = fmap (videoId . itemVideo . snd) . L.listSelectedElement . sVideosL $ s
    logF (i, p, m) = sendChan (DownloadProgress i p m) s

downloadProgressH :: VideoId -> Maybe Progress -> String -> State -> T.EventM ResourceName (T.Next State)
downloadProgressH vId mp _ s = M.continue case mp of
  Nothing -> s
  Just p -> updateVideoL (\i -> i {itemProgress = Just p}) vId s

-- TODO: styling
logH :: String -> LogLevel -> State -> T.EventM ResourceName (T.Next State)
logH m l s = M.continue $ s {sStatus = m, sLog = sLog s ++ [(l, m)]}

resizeH :: State -> T.EventM ResourceName (T.Next State)
resizeH s = do
  w <- videoListWidth s
  M.continue (s {sVideosLWidth = w})

videoListWidth :: State -> T.EventM ResourceName Int
videoListWidth s = do
  e <- M.lookupExtent VideoList
  case e of
    Nothing -> return . sVideosLWidth $ s
    Just (T.Extent _ _ (width, _)) -> return width

loadFromDump :: State -> IO (Maybe ([Channel], [Video]))
loadFromDump s = do
  mChs <- loadChannels (channelsDumpPath . sSettings $ s)
  mVs <- loadVideos (videosDumpPath . sSettings $ s)
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
