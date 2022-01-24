{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Ytm.Api.Video where

import Control.Lens
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Internal as BS
import Data.List (sortOn)
import Data.Maybe (isJust)
import qualified Data.Ord as O
import qualified Data.Text as T
import Data.Time (UTCTime)
import Network.Wreq
import Ytm.Api
import qualified Ytm.Api.Channel as C
import Ytm.Api.Time

subscriptionsVideos :: UTCTime -> Credentials -> IO [Video]
subscriptionsVideos publishedAfter c = do
  chs <- C.subscriptions c
  sortOn (O.Down . publishedAt) . concat <$> mapM (\ch -> channelVideos publishedAfter ch c) chs

channelVideos :: UTCTime -> Channel -> Credentials -> IO [Video]
channelVideos = channelVideos' Nothing []
  where
    channelVideos' npt ls daysB ch c = do
      (l, mn) <- channelVideosPage npt ch c
      nl <-
        if isJust mn && all daysBP l
          then channelVideos' mn (ls ++ l) daysB ch c
          else return (ls ++ l)
      return . filter daysBP $ nl
      where
        daysBP v = publishedAt v > daysB

channelVideosPage :: Maybe PageToken -> Channel -> Credentials -> IO ([Video], Maybe PageToken)
channelVideosPage npt ch c = do
  let opts =
        defaults
          & authorizationHeader c
          & acceptJsonHeader
          & paramString "key" (clientId c)
          & paramString "playlistId" (channelUploadsPlaylistId ch)
          & paramString "part" "snippet,id,contentDetails"
          & paramString "maxResults" "50"
      d = domain ++ "playlistItems?"
      nptP = maybe "" ("&pageToken=" ++) npt
      url = d ++ nptP
  r <- getWith opts url
  let vs = fromResponse ch r
  return vs

fromResponse :: Channel -> Response BS.ByteString -> ([Video], Maybe PageToken)
fromResponse ch r = (videos, nextPageToken)
  where
    parseR r' k = T.unpack <$> r' ^.. responseBody . key "items" . values . k . _String
    videoIds = parseR r (key "snippet" . key "resourceId" . key "videoId")
    videoTitles = parseR r (key "snippet" . key "title")
    videoPublishedAts = parseR r (key "contentDetails" . key "videoPublishedAt")
    videos = zipWith3 (Video ch) videoIds (map (readUTCTime utcTimeFormat) videoPublishedAts) videoTitles
    nextPageToken = T.unpack <$> r ^? responseBody . key "nextPageToken" . _String
