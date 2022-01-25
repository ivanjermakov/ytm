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
    sVideosL :: L.List () Video
  }

data Settings = Settings
  { fetchDays :: Int,
    videosDumpPath :: FilePath,
    channelsDumpPath :: FilePath
  }
  deriving (Show)

instance Show State where
  show (State set _ st c ch lCh v vl) = "State " ++ show (set, st, c, ch, lCh, v, vl)

data CustomEvent
  = CredentialsLoaded Credentials
  | DumpLoaded ([Channel], [Video])
  | ChannelsLoaded [Channel]
  | ChannelVideosLoaded [Video]
  | VideosLoaded
  deriving (Show)
