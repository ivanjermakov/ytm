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
    sVideosL :: L.List ResourceName Video,
    sVideosLWidth :: Int,
    sLog :: [(LogLevel, String)]
  }

data LogLevel = INFO | WARN | ERROR
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
  | VideoDownloaded FilePath
  | Log String LogLevel
  deriving (Show)

data ResourceName = VideoList
  deriving (Eq, Ord, Show)
