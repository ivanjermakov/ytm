module Ytm.App.Types where

import Brick.BChan (BChan)
import qualified Brick.Widgets.List as L
import Ytm.Api

data State = State
  { sSettings :: Settings,
    bChan :: BChan CustomEvent,
    sStatus :: String,
    sCredentials :: Maybe Credentials,
    sChannels :: [Channel],
    sLoadedChannels :: Int,
    sVideos :: [Video],
    sVideosL :: L.List ResourceName VideoItem,
    sVideosLWidth :: Int,
    sLog :: [(LogLevel, String)]
  }

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
    channelsDumpPath :: FilePath
  }
  deriving (Show)

data CustomEvent
  = CredentialsLoaded Credentials
  | DumpLoaded ([Channel], [Video])
  | ChannelsLoaded [Channel]
  | ChannelVideosLoaded [Video]
  | VideosLoaded
  | VideoDownloaded VideoId FilePath
  | DownloadProgress VideoId (Maybe Progress) String
  | Log String LogLevel
  deriving (Show)

data ResourceName = VideoList
  deriving (Eq, Ord, Show)

type Progress = Float
