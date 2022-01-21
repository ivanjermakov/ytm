{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Configuration.Dotenv as C
import Control.Lens
import Data.Aeson.Lens
import Data.String (fromString)
import qualified Data.Text as T
import Network.Wreq
import System.Environment (getEnv)
import Text.Printf (printf)

data Credentials = Credentials {apiKey :: String, clientId :: String, accessToken :: String}
  deriving (Show)

type ChannelId = String

credentials :: IO Credentials
credentials = do
  k <- getApiKey
  cId <- getClientId
  t <- getAccessToken
  return $ Credentials k cId t
  where
    getApiKey = getEnv "API_KEY"
    getClientId = getEnv "CLIENT_ID"
    getAccessToken = getEnv "ACCESS_TOKEN"

main :: IO ()
main = do
  _ <- C.loadFile C.defaultConfig
  k <- credentials
  print k
  ss <- subscriptions k
  print ss
  print $ length ss
  return ()

subscriptions :: Credentials -> IO ([T.Text], Maybe T.Text)
subscriptions c = do
  let opts = defaults & authorizationHeader c & acceptJsonHeader
  r <- getWith opts (printf "https://youtube.googleapis.com/youtube/v3/subscriptions?part=snippet%%2CcontentDetails&maxResults=1000&mine=true&key=%s" (clientId c))
  let channelIds = r ^.. responseBody . key "items" . values . key "snippet" . key "channelId" . _String
  let nextPageToken = r ^? responseBody . key "nextPageToken" . _String
  return (channelIds, nextPageToken)

acceptJsonHeader :: Options -> Options
acceptJsonHeader = header "Accept" .~ ["application/json"]

authorizationHeader :: Credentials -> Options -> Options
authorizationHeader c = header "Authorization" .~ [fromString $ "Bearer " ++ accessToken c]
