{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Ytm.Api.Video where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (catch)
import qualified Control.Exception as E
import Control.Exception.Base
import Control.Lens
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Internal as BS
import Data.Either (fromRight)
import Data.List (sortOn, zipWith6)
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Ord as O
import qualified Data.Text as T
import Data.Time (UTCTime, ctTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Network.Wreq
import Ytm.Api
import qualified Ytm.Api.Channel as C

subscriptionsVideos :: UTCTime -> Credentials -> IO [Video]
subscriptionsVideos publishedAfter c = do
  chs <- C.subscriptions c
  sortOn (O.Down . publishedAt) . concat <$> mapConcurrently getVideos chs
  where
    getVideos ch = do
      fromRight [] <$> (E.try (channelVideos publishedAfter ch c) :: IO (Either E.SomeException [Video]))

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
  (vIds, npt') <- channelVideoIdsPage npt ch c
  let opts =
        defaults
          & authorizationHeader c
          & acceptJsonHeader
          & paramString "key" (clientId c)
          & paramStrings "id" vIds
          & paramString "part" "contentDetails,statistics,snippet"
          & paramString "maxResults" "50"
      d = domain ++ "videos?"
      nptP = maybe "" ("&pageToken=" ++) npt
      url = d ++ nptP
  r <- getWith opts url
  return (fromResponse ch r, npt')

channelVideoIdsPage :: Maybe PageToken -> Channel -> Credentials -> IO ([VideoId], Maybe PageToken)
channelVideoIdsPage npt ch c = do
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
  let videoIds = T.unpack <$> r ^.. responseBody . key "items" . values . (key "snippet" . key "resourceId" . key "videoId") . _String
  let nextPageToken = T.unpack <$> r ^? responseBody . key "nextPageToken" . _String
  return (videoIds, nextPageToken)

fromResponse :: Channel -> Response BS.ByteString -> [Video]
fromResponse ch r = videos
  where
    parseR r' k = T.unpack <$> r' ^.. responseBody . key "items" . values . k . _String
    vIds = parseR r (key "id")
    vPublishedAts = parseR r (key "snippet" . key "publishedAt")
    vTitles = parseR r (key "snippet" . key "title")
    vDescriptions = parseR r (key "snippet" . key "description")
    vDurations = parseR r (key "contentDetails" . key "duration")
    vViews = parseR r (key "statistics" . key "viewCount")
    videos =
      zipWith6
        (Video ch)
        vIds
        (mapMaybe iso8601ParseM vPublishedAts)
        vTitles
        vDescriptions
        (map ctTime . mapMaybe iso8601ParseM $ vDurations)
        (map read vViews)
