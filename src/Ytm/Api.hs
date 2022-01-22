{-# LANGUAGE OverloadedStrings #-}

module Ytm.Api where

import Control.Lens
import Data.Aeson.Lens
import Data.Maybe (isJust)
import Data.String (fromString)
import qualified Data.Text as T
import Network.Wreq
import System.Environment (getEnv)

data Credentials = Credentials {apiKey :: String, clientId :: String, accessToken :: String, refreshToken :: String}
  deriving (Show)

data Channel = Channel {channelId :: String, channelName :: String}
  deriving (Show)

type PageToken = T.Text

credentials :: IO Credentials
credentials = do
  k <- getEnv "API_KEY"
  cId <- getEnv "CLIENT_ID"
  cs <- getEnv "CLIENT_SECRET"
  rt <- getEnv "REFRESH_TOKEN"
  let url = "https://www.googleapis.com/oauth2/v4/token"
  r <- post url ["client_id" := cId, "client_secret" := cs, "refresh_token" := rt, "grant_type" := ("refresh_token" :: String)]
  let accT = T.unpack $ r ^. responseBody . key "access_token" . _String
  return $ Credentials k cId accT rt

subscriptions :: Maybe PageToken -> Credentials -> IO [Channel]
subscriptions npt c = do
  (l, mn) <- subscriptions' npt c
  nl <- if isJust mn then subscriptions mn c else return l
  return $ l ++ nl

-- no nextPageToken means first page
subscriptions' :: Maybe PageToken -> Credentials -> IO ([Channel], Maybe PageToken)
subscriptions' npt c = do
  let opts = defaults & authorizationHeader c & acceptJsonHeader
  let domain = "https://youtube.googleapis.com/youtube/v3/subscriptions"
  let qP = "?part=snippet%2CcontentDetails&maxResults=1000&mine=true&key" ++ clientId c
  let nptP = maybe "" (\t -> "&pageToken=" ++ T.unpack t) npt
  let url = concat [domain, qP, nptP]
  r <- getWith opts url
  let channelIds = parseR r (key "channelId")
  let channelNames = parseR r (key "title")
  let channels = zipWith Channel channelIds channelNames
  let nextPageToken = r ^? responseBody . key "nextPageToken" . _String
  return (channels, nextPageToken)
  where
    parseR r' k = T.unpack <$> r' ^.. responseBody . key "items" . values . key "snippet" . k . _String

acceptJsonHeader :: Options -> Options
acceptJsonHeader = header "Accept" .~ ["application/json"]

authorizationHeader :: Credentials -> Options -> Options
authorizationHeader c = header "Authorization" .~ [fromString $ "Bearer " ++ accessToken c]
