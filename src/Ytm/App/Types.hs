{-# LANGUAGE DeriveGeneric #-}

module Ytm.App.Types where

import Brick.BChan (BChan)
import qualified Brick.Widgets.List as L
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Ytm.Api

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
  | DownloadProgress VideoId (Maybe Progress) String
  | Log String LogLevel
  | FsChanged
  deriving (Show)

data ResourceName = VideoList
  deriving (Eq, Ord, Show)

type Progress = Float
