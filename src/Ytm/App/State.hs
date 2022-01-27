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
import Ytm.Util.Persistence

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
      sStatus = "ytm started",
      sCredentials = Nothing,
      sChannels = [],
      sLoadedChannels = 0,
      sVideos = [],
      sVideosL = L.list VideoList (Vec.fromList []) 1,
      sVideosLWidth = 0
    }

handleEvent :: State -> T.BrickEvent ResourceName CustomEvent -> T.EventM ResourceName (T.Next State)
handleEvent s e = case e of
  T.AppEvent cusE -> case cusE of
    (CredentialsLoaded c) -> handleCredentialsLoaded c s
    DumpLoaded d -> handleDumpLoaded d s
    (ChannelsLoaded chs) -> handleChannelsLoaded chs s
    (ChannelVideosLoaded vs) -> handleChannelVideosLoaded vs s
    VideosLoaded -> handleVideosLoaded s
  T.VtyEvent k -> case k of
    V.EvKey (V.KChar 'q') [] -> M.halt s
    V.EvKey (V.KChar 'i') [] -> moveBy (-1)
    V.EvKey (V.KChar 'k') [] -> moveBy 1
    V.EvKey (V.KChar 'r') [] -> handleLoadVideos s
    V.EvResize _ _ -> handleResize s
    _ -> handleL
    where
      moveBy n = M.continue . (\l -> s {sVideosL = l}) . L.listMoveBy n =<< L.handleListEvent k (sVideosL s)
      handleL = M.continue . (\l -> s {sVideosL = l}) =<< L.handleListEvent k (sVideosL s)

handleCredentialsLoaded :: Credentials -> State -> T.EventM ResourceName (T.Next State)
handleCredentialsLoaded c s = do
  void . liftIO . forkIO $ do
    l <- loadFromDump s
    when (isJust l) $ writeBChan (bChan s) (DumpLoaded $ fromJust l)
  M.continue
    ( s
        { sCredentials = Just c,
          sStatus = "credentials loaded"
        }
    )

handleDumpLoaded :: ([Channel], [Video]) -> State -> T.EventM ResourceName (T.Next State)
handleDumpLoaded (chs, vs) s = do
  liftIO $ writeBChan (bChan s) VideosLoaded
  M.continue
    ( s
        { sChannels = chs,
          sVideos = vs,
          sStatus = "videos loaded from cache DATE"
        }
    )

handleChannelsLoaded :: [Channel] -> State -> T.EventM ResourceName (T.Next State)
handleChannelsLoaded chs s =
  M.continue
    ( s
        { sChannels = chs,
          sStatus = "channels loaded: " ++ show (length chs)
        }
    )

handleChannelVideosLoaded :: [Video] -> State -> T.EventM ResourceName (T.Next State)
handleChannelVideosLoaded vs s =
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

handleVideosLoaded :: State -> T.EventM ResourceName (T.Next State)
handleVideosLoaded s = do
  dumpVs
  w <- videoListWidth s
  M.continue (s {sVideosL = L.list VideoList (Vec.fromList sortVs) 1, sVideosLWidth = w})
  where
    sortVs = sortOn (O.Down . publishedAt) . sVideos $ s
    dumpVs = liftIO $ dump (videosDumpPath . sSettings $ s) (sVideos s)

handleLoadVideos :: State -> T.EventM ResourceName (T.Next State)
handleLoadVideos s = do
  let ns = (s {sStatus = "refreshing videos"})
      c = fromJust . sCredentials $ s
  void . liftIO . forkIO $ do
    chs <- subscriptions c
    dumpChs chs
    writeBChan (bChan s) (ChannelsLoaded chs)
    void $ mapConcurrently (chLoaded c) chs
    writeBChan (bChan s) VideosLoaded
  M.continue ns
  where
    dumpChs = dump (channelsDumpPath . sSettings $ s)
    chLoaded c ch = do
      db <- daysBefore (fetchDays . sSettings $ s)
      cVs <- channelVideos db ch c
      writeBChan (bChan s) (ChannelVideosLoaded cVs)

handleResize :: State -> T.EventM ResourceName (T.Next State)
handleResize s = do
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
