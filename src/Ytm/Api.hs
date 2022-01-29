{-# LANGUAGE OverloadedStrings #-}

module Ytm.Api where

import Control.Lens
import Data.Aeson.Lens
import Data.String (fromString)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime)
import Network.Wreq
import System.Environment (getEnv)

data Credentials = Credentials
  { apiKey :: String,
    clientId :: String,
    accessToken :: String,
    refreshToken :: String
  }
  deriving (Show)

data Channel = Channel
  { channelId :: ChannelId,
    channelName :: String,
    channelUploadsPlaylistId :: PlaylistId
  }
  deriving (Show, Read, Eq, Ord)

data Video = Video
  { channel :: Channel,
    videoId :: VideoId,
    publishedAt :: UTCTime,
    videoTitle :: String,
    videoDescription :: String,
    videoDuration :: NominalDiffTime,
    videoViews :: Int
  }
  deriving (Show, Read, Eq, Ord)

type PageToken = String

type VideoId = String

type ChannelId = String

type PlaylistId = String

--TODO: exception handling
credentials :: IO Credentials
credentials = do
  k <- getEnv "API_KEY"
  cId <- getEnv "CLIENT_ID"
  cs <- getEnv "CLIENT_SECRET"
  rt <- getEnv "REFRESH_TOKEN"
  let url = "https://www.googleapis.com/oauth2/v4/token"
      body =
        [ "client_id" := cId,
          "client_secret" := cs,
          "refresh_token" := rt,
          "grant_type" := ("refresh_token" :: String)
        ]
  print body
  r <- post url body
  let accT = T.unpack $ r ^. responseBody . key "access_token" . _String
  return $ Credentials k cId accT rt

domain :: String
domain = "https://youtube.googleapis.com/youtube/v3/"

paramString :: String -> String -> Options -> Options
paramString k v = param (T.pack k) .~ [T.pack v]

paramStrings :: String -> [String] -> Options -> Options
paramStrings k v = param (T.pack k) .~ map T.pack v

acceptJsonHeader :: Options -> Options
acceptJsonHeader = header "Accept" .~ ["application/json"]

authorizationHeader :: Credentials -> Options -> Options
authorizationHeader c = header "Authorization" .~ [fromString $ "Bearer " ++ accessToken c]
