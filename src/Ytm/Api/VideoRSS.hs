{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Ytm.Api.VideoRSS where

import Control.Lens
import Data.Aeson.Lens
import qualified Data.Text as T
import Data.Time.Clock
import System.Directory (removeFile)
import System.Process (readCreateProcess, readProcess, shell)
import Ytm.Api
import qualified Ytm.Api.Channel as C
import Ytm.Api.Time

subscriptionsVideos :: UTCTime -> Credentials -> IO [Video]
subscriptionsVideos publishedAfter c = do
  ss <- C.subscriptions c
  vs <- mapM channelVideos ss
  return $ concatMap (filter (\v -> publishedAt v > publishedAfter)) vs

channelVideos :: Channel -> IO [Video]
channelVideos ch = do
  let url = "https://www.youtube.com/feeds/videos.xml?channel_id=" ++ channelId ch
  xml <- readProcess "curl" ["-s", url] ""
  json <- xmlToJson xml
  return $ fromResponse ch json

fromResponse :: Channel -> String -> [Video]
fromResponse ch json = videos
  where
    parseR j k = T.unpack <$> j ^.. key "feed" . key "entry" . values . k . _String
    videoIds = parseR json (key "yt:videoId")
    videoTitles = parseR json (key "title")
    videoPublishedAts = parseR json (key "published")
    videos = zipWith3 (Video ch) videoIds (map (readUTCTime utcTimeFormatWithTz) videoPublishedAts) videoTitles
    nextPageToken = json ^? key "nextPageToken" . _String

xmlToJson :: String -> IO String
xmlToJson xml = do
  writeFile "x.xml" xml
  json <- readCreateProcess (shell "cat x.xml | xq .") ""
  removeFile "x.xml"
  return json
