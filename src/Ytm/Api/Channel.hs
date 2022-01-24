{-# LANGUAGE OverloadedStrings #-}

module Ytm.Api.Channel where

import Control.Lens
import Data.Aeson.Lens
import qualified Data.List.Split as S
import Data.Maybe (isJust)
import qualified Data.Text as T
import Network.Wreq
import Ytm.Api

subscriptions :: Credentials -> IO [Channel]
subscriptions c = do
  ss <- channelIds' Nothing c
  fmap concat . mapM (`channelsFromIds` c) . S.chunksOf 50 $ ss
  where
    channelIds' npt c' = do
      (l, mn) <- channelIdsPage npt c'
      nl <- if isJust mn then channelIds' mn c' else return l
      return $ l ++ nl

channelIdsPage :: Maybe PageToken -> Credentials -> IO ([ChannelId], Maybe PageToken)
channelIdsPage npt c = do
  let opts =
        defaults
          & authorizationHeader c
          & acceptJsonHeader
          & paramString "part" "snippet,contentDetails"
          & paramString "maxResults" "50"
          & paramString "mine" "true"
          & paramString "key" (clientId c)
      d = domain ++ "subscriptions?"
      nptP = maybe "" ("&pageToken=" ++) npt
      url = d ++ nptP
  r <- getWith opts url
  let ids = T.unpack <$> r ^.. responseBody . key "items" . values . key "snippet" . key "resourceId" . key "channelId" . _String
      nextPageToken = T.unpack <$> r ^? responseBody . key "nextPageToken" . _String
  return (ids, nextPageToken)

channelsFromIds :: [ChannelId] -> Credentials -> IO [Channel]
channelsFromIds ids c = do
  let opts =
        defaults
          & authorizationHeader c
          & acceptJsonHeader
          & paramString "key" (clientId c)
          & paramStrings "id" ids
          & paramString "part" "id,snippet,contentDetails"
      url = domain ++ "channels?"
  r <- getWith opts url
  let chs =
        zipWith3
          Channel
          (T.unpack <$> r ^.. responseBody . key "items" . values . key "id" . _String)
          (T.unpack <$> r ^.. responseBody . key "items" . values . key "snippet" . key "title" . _String)
          (T.unpack <$> r ^.. responseBody . key "items" . values . key "contentDetails" . key "relatedPlaylists" . key "uploads" . _String)
  return chs
