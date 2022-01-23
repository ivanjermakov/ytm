{-# LANGUAGE OverloadedStrings #-}

module Ytm.Api where

import Control.Lens
import Data.Aeson.Lens
import Data.Map (Map)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
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
  { channelId :: String,
    channelName :: String
  }
  deriving (Show, Eq, Ord)

data Video = Video
  { channel :: Channel,
    videoId :: String,
    publishedAt :: UTCTime,
    videoTitle :: String
  }
  deriving (Show, Eq, Ord)

type SubscriptionsVideoMap = Map Channel [Video]

type PageToken = Text

credentials :: IO Credentials
credentials = do
  k <- getEnv "API_KEY"
  cId <- getEnv "CLIENT_ID"
  cs <- getEnv "CLIENT_SECRET"
  rt <- getEnv "REFRESH_TOKEN"
  let url = "https://www.googleapis.com/oauth2/v4/token"
  r <-
    post
      url
      [ "client_id" := cId,
        "client_secret" := cs,
        "refresh_token" := rt,
        "grant_type" := ("refresh_token" :: String)
      ]
  let accT = T.unpack $ r ^. responseBody . key "access_token" . _String
  return $ Credentials k cId accT rt

domain :: String
domain = "https://youtube.googleapis.com/youtube/v3/"

paramString :: String -> String -> Options -> Options
paramString k v = param (T.pack k) .~ [T.pack v]

acceptJsonHeader :: Options -> Options
acceptJsonHeader = header "Accept" .~ ["application/json"]

authorizationHeader :: Credentials -> Options -> Options
authorizationHeader c = header "Authorization" .~ [fromString $ "Bearer " ++ accessToken c]