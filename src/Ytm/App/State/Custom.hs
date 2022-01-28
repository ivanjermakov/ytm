{-# LANGUAGE BlockArguments #-}

module Ytm.App.State.Custom where

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
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
  sendChan (Log "credentials loaded" Info) s
  M.continue (s {sCredentials = Just c})

settingsLoadedH :: Settings -> State -> T.EventM ResourceName (T.Next State)
settingsLoadedH settings s = do
  sendChan (Log "settings loaded" Info) s'
  async $ do
    l <- loadFromDump s'
    when (isJust l) $ sendChan (DumpLoaded $ fromJust l) s'
  M.continue s'
  where
    s' = s {sSettings = Just settings}

fsChangedH :: State -> T.EventM ResourceName (T.Next State)
fsChangedH s = do
  files <- liftIO . listDownloadedFiles . downloadedPath . fromJust . sSettings $ s
  M.continue $
    s
      { sDownloadedFiles = files,
        sVideosL =
          -- TODO: treat part files
          fmap
            ( \i ->
                if isDownloaded files . videoId . itemVideo $ i
                  then i {itemProgress = Just 100, itemStatus = Downloaded}
                  else i
            )
            . sVideosL
            $ s
      }
  where
    isDownloaded files vId = any (`match` vId) files
    match :: FilePath -> VideoId -> Bool
    match f vId = f =~ (printf "^%s\\..*" vId :: String)

dumpLoadedH :: ([Channel], [Video]) -> State -> T.EventM ResourceName (T.Next State)
dumpLoadedH (chs, vs) s = do
  sendChan VideosLoaded s
  M.continue
    ( s
        { sChannels = chs,
          sVideos = vs,
          sStatus = "videos loaded from cache"
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
      (\i -> i {itemProgress = Just 100, itemStatus = Downloaded})
      vId
      s

downloadProgressH :: VideoId -> Maybe Progress -> String -> State -> T.EventM ResourceName (T.Next State)
downloadProgressH vId mp _ s = M.continue case mp of
  Nothing -> s
  Just p -> updateVideoL (\i -> i {itemProgress = Just p}) vId s

-- TODO: styling
logH :: String -> LogLevel -> State -> T.EventM ResourceName (T.Next State)
logH m l s = M.continue $ s {sStatus = m, sLog = sLog s ++ [(l, m)]}
