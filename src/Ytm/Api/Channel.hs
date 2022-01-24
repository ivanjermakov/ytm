{-# LANGUAGE OverloadedStrings #-}

module Ytm.Api.Channel where

import Control.Lens
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Internal as BS
import qualified Data.List.Split as S
import Data.Maybe (isJust)
import qualified Data.Text as T
import Network.Wreq
import Ytm.Api
import Ytm.Util.List (mergeDropUnmatched)

subscriptions :: Credentials -> IO [Channel]
subscriptions c = do
  ss <- subscriptions' Nothing c
  fmap concat . mapM (`channelsUpdatePlaylistId` c) . S.chunksOf 50 $ ss
  where
    subscriptions' npt c' = do
      (l, mn) <- subscriptionsPage npt c'
      nl <- if isJust mn then subscriptions' mn c' else return l
      return $ l ++ nl

subscriptionsPage :: Maybe PageToken -> Credentials -> IO ([Channel], Maybe PageToken)
subscriptionsPage npt c = do
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
  return $ fromResponse r

fromResponse :: Response BS.ByteString -> ([Channel], Maybe PageToken)
fromResponse r = (channels, nextPageToken)
  where
    parseR r' k = T.unpack <$> r' ^.. responseBody . key "items" . values . key "snippet" . k . _String
    channelIds = parseR r (key "resourceId" . key "channelId")
    channelNames = parseR r (key "title")
    channels = zipWith (\i n -> Channel i n Nothing) channelIds channelNames
    nextPageToken = T.unpack <$> r ^? responseBody . key "nextPageToken" . _String

channelsUpdatePlaylistId :: [Channel] -> Credentials -> IO [Channel]
channelsUpdatePlaylistId chs c = do
  let opts =
        defaults
          & authorizationHeader c
          & acceptJsonHeader
          & paramString "key" (clientId c)
          & paramStrings "id" (map channelId chs)
          & paramString "part" "contentDetails"
      url = domain ++ "channels?"
  r <- getWith opts url
  let pIds =
        zip
          (T.unpack <$> r ^.. responseBody . key "items" . values . key "id" . _String)
          (T.unpack <$> r ^.. responseBody . key "items" . values . key "contentDetails" . key "relatedPlaylists" . key "uploads" . _String)
  return $
    mergeDropUnmatched
      (\ch (_, i) -> ch {channelUploadsPlaylistId = Just i})
      (\ch (chId, _) -> channelId ch == chId)
      chs
      pIds
