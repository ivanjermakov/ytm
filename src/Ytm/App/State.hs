{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Ytm.App.State where

import Brick.BChan (BChan, writeBChan)
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.List (sortOn)
import qualified Data.Ord as O
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Text.Printf (printf)
import Ytm.Api
import Ytm.Api.Channel (subscriptions)
import Ytm.Api.Time (daysBefore)
import Ytm.Api.Video (channelVideos)
import Ytm.App.Types

initState :: BChan CustomEvent -> State
initState ch =
  State
    { sSettings = Settings 4,
      bChan = ch,
      sStatus = "ytm started",
      sCredentials = Nothing,
      sChannels = [],
      sLoadedChannels = 0,
      sVideos = [],
      sVideosL = L.list () (Vec.fromList []) 1
    }

handleEvent :: State -> T.BrickEvent () CustomEvent -> T.EventM () (T.Next State)
handleEvent s e = case e of
  T.AppEvent cusE -> case cusE of
    (CredentialsLoaded c) -> handleCredentialsLoaded c s
    (ChannelsLoaded chs) -> M.continue (s {sChannels = chs, sStatus = "channels loaded: " ++ show (length chs)})
    (ChannelVideosLoaded vs) -> handleChannelVideosLoaded vs s
    VideosLoaded -> handleVideosLoaded s
  T.VtyEvent k -> case k of
    V.EvKey (V.KChar 'q') [] -> M.halt s
    V.EvKey (V.KChar 'i') [] -> moveBy (-1)
    V.EvKey (V.KChar 'k') [] -> moveBy 1
    _ -> handleL
    where
      moveBy n = M.continue . (\l -> s {sVideosL = l}) . L.listMoveBy n =<< L.handleListEvent k (sVideosL s)
      handleL = M.continue . (\l -> s {sVideosL = l}) =<< L.handleListEvent k (sVideosL s)

handleVideosLoaded :: State -> T.EventM () (T.Next State)
handleVideosLoaded s = M.continue (s {sVideosL = L.list () (Vec.fromList sortVs) 1})
  where
    sortVs = sortOn (O.Down . publishedAt) . sVideos $ s

handleCredentialsLoaded :: Credentials -> State -> T.EventM () (T.Next State)
handleCredentialsLoaded c s = do
  let ns = (s {sCredentials = Just c, sStatus = "loading channels"})
  void . liftIO . forkIO $ do
    chs <- subscriptions c
    writeBChan (bChan s) (ChannelsLoaded chs)
    void $ mapConcurrently chLoaded chs
    writeBChan (bChan s) VideosLoaded
  M.continue ns
  where
    chLoaded ch = do
      db <- daysBefore (fetchDays . sSettings $ s)
      cVs <- channelVideos db ch c
      writeBChan (bChan s) (ChannelVideosLoaded cVs)

handleChannelVideosLoaded :: [Video] -> State -> T.EventM () (T.Next State)
handleChannelVideosLoaded vs s =
  M.continue
    ( s
        { sVideos = sVideos s ++ vs,
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
