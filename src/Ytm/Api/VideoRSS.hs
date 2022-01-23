{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Ytm.Api.VideoRSS where

import Control.Lens
import Data.Aeson.Lens
import qualified Data.List.Split as S
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time.Clock
import System.Process (readProcess)
import Ytm.Api
import Ytm.Api.Channel
import Ytm.Api.Time

subscriptionsVideos :: UTCTime -> Credentials -> IO SubscriptionsVideoMap
subscriptionsVideos publishedAfter c = do
  ss <- subscriptions c
  vs <- mapM channelVideos ss
  return $
    M.fromList
      . map ((\vs' -> (channel (head vs'), vs')) . filter (\v -> publishedAt v > publishedAfter))
      $ vs

channelVideos :: Channel -> IO [Video]
channelVideos ch = do
  let url = "https://feed2json.org/convert?url=https%3A%2F%2Fwww.youtube.com%2Ffeeds%2Fvideos.xml%3Fchannel_id%3D" ++ channelId ch
  json <- readProcess "curl" ["-s", url] ""
  return $ fromResponse ch json

fromResponse :: Channel -> String -> [Video]
fromResponse ch json = videos
  where
    parseR j k = T.unpack <$> j ^.. key "items" . values . k . _String
    videoIds = parseR json (key "url")
    videoTitles = parseR json (key "title")
    videoPublishedAts = parseR json (key "date_published")
    videos = zipWith3 (Video ch) (concatMap (tail . S.splitOn "=") videoIds) (map (readUTCTime utcTimeFormatWithMillis) videoPublishedAts) videoTitles
    nextPageToken = json ^? key "nextPageToken" . _String
