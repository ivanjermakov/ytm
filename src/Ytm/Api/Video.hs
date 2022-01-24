{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Ytm.Api.Video where

import Control.Lens
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Internal as BS
import Data.List (sortOn)
import qualified Data.Map as M
import Data.Maybe (isJust)
import qualified Data.Ord as O
import qualified Data.Text as T
import Data.Time (UTCTime)
import Network.Wreq
import Ytm.Api
import qualified Ytm.Api.Channel as C
import Ytm.Api.Time

subscriptionsVideos :: UTCTime -> Credentials -> IO SubscriptionsVideoMap
subscriptionsVideos publishedAfter c = do
  ss <- C.subscriptions c
  vs <- mapM (\ch -> channelVideos publishedAfter ch c) ss
  let svm = M.fromList . map (\vs' -> (channel (head vs'), vs')) $ vs
  return svm

channelVideos :: UTCTime -> Channel -> Credentials -> IO [Video]
channelVideos = channelVideos' Nothing []
  where
    channelVideos' npt ls daysB ch c = do
      (l, mn) <- channelVideosPage npt daysB ch c
      nl <- if isJust mn then channelVideos' mn (ls ++ l) daysB ch c else return (ls ++ l)
      return . sortOn (O.Down . publishedAt) $ nl

channelVideosPage :: Maybe PageToken -> UTCTime -> Channel -> Credentials -> IO ([Video], Maybe PageToken)
channelVideosPage npt publishedAfter ch c = do
  let opts =
        defaults
          & authorizationHeader c
          & acceptJsonHeader
          & paramString "key" (clientId c)
          & paramString "channelId" (channelId ch)
          & paramString "publishedAfter" (showUTCTime utcTimeFormat publishedAfter)
  let d = domain ++ "search"
  let qP = "?part=snippet%2Cid&maxResults=50&order=date&safeSearch=none"
  let nptP = maybe "" ("&pageToken=" ++) npt
  let url = concat [d, qP, nptP]
  r <- getWith opts url
  let vs = fromResponse ch r
  return vs

fromResponse :: Channel -> Response BS.ByteString -> ([Video], Maybe PageToken)
fromResponse ch r = (videos, nextPageToken)
  where
    parseR r' k = T.unpack <$> r' ^.. responseBody . key "items" . values . k . _String
    videoIds = parseR r (key "id" . key "videoId")
    videoTitles = parseR r (key "snippet" . key "title")
    videoPublishedAts = parseR r (key "snippet" . key "publishedAt")
    videos = zipWith3 (Video ch) videoIds (map (readUTCTime utcTimeFormat) videoPublishedAts) videoTitles
    nextPageToken = T.unpack <$> r ^? responseBody . key "nextPageToken" . _String

channelPlaylistId :: Channel -> Credentials -> IO PlaylistId
channelPlaylistId ch c = do
  let opts =
        defaults
          & authorizationHeader c
          & acceptJsonHeader
          & paramString "key" (clientId c)
          & paramString "id" (channelId ch)
          & paramString "part" "contentDetails"
  let url = domain ++ "channels?"
  r <- getWith opts url
  let pId =
        head $
          T.unpack <$> r
            ^.. responseBody
              . key "items"
              . values
              . key "contentDetails"
              . key "relatedPlaylists"
              . key "uploads"
              . _String
  return pId
