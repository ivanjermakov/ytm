{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ytm.App.Types where

import Brick.BChan (BChan)
import qualified Brick.Widgets.List as L
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Ytm.Api
import Ytm.Util.Range

data State = State
  { sSettings :: Maybe Settings,
    bChan :: BChan CustomEvent,
    sStatus :: String,
    sCredentials :: Maybe Credentials,
    sChannels :: [Channel],
    sLoadedChannels :: Int,
    sVideos :: [Video],
    sVideosL :: L.List ResourceName VideoItem,
    sVideosLWidth :: Int,
    sDownloadedFiles :: [FilePath],
    sLog :: [(LogLevel, String)],
    sSelectMode :: Maybe SelectMode
  }

data SelectMode = SelectMode
  { selectStart :: Int,
    selectEnd :: Int
  }
  deriving (Show, Read, Eq, Ord)

instance Range SelectMode Int where
  inRange a (SelectMode start end) = a >= minimum ls && a <= maximum ls
    where
      ls = [start, end]

data VideoItem = VideoItem
  { itemVideo :: Video,
    itemProgress :: Maybe Progress,
    itemStatus :: VideoStatus
  }
  deriving (Show, Read, Eq, Ord)

data VideoStatus = Available | Downloading | Downloaded
  deriving (Show, Read, Eq, Ord)

data LogLevel = Info | Warn | Error
  deriving (Show, Read, Eq, Ord)

data Settings = Settings
  { fetchDays :: Int,
    videosDumpPath :: FilePath,
    channelsDumpPath :: FilePath,
    downloadedPath :: FilePath,
    downloadCommandPattern :: String,
    playCommandPattern :: String
  }
  deriving (Show, Generic)

instance FromJSON Settings

data CustomEvent
  = CredentialsLoaded Credentials
  | SettingsLoaded Settings
  | DumpLoaded ([Channel], [Video])
  | ChannelsLoaded [Channel]
  | ChannelVideosLoaded [Video]
  | VideosLoaded
  | VideoDownloaded VideoId FilePath
  | DownloadProgress VideoId Progress
  | Log String LogLevel
  | FsChanged
  deriving (Show)

data ResourceName = VideoList
  deriving (Eq, Ord, Show)

data Progress = Progress
  { status :: VideoStatus,
    downPercent :: Float,
    downBytes :: Int,
    totalBytes :: Int,
    speed :: Maybe Float,
    elapsed :: Maybe Float,
    eta :: Maybe Float
  }
  deriving (Show, Read, Eq, Ord)

progressDownloaded :: Progress
progressDownloaded = Progress Downloaded 100 0 0 Nothing Nothing Nothing
