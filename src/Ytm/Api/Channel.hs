{-# LANGUAGE OverloadedStrings #-}

module Ytm.Api.Channel where

import Control.Lens
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Internal as BS
import Data.Maybe (isJust)
import qualified Data.Text as T
import Network.Wreq
import Ytm.Api

subscriptions :: Credentials -> IO [Channel]
subscriptions = subscriptions' Nothing
  where
    subscriptions' npt c = do
      (l, mn) <- subscriptionsPage npt c
      nl <- if isJust mn then subscriptions' mn c else return l
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
  let d = domain ++ "subscriptions?"
  let nptP = maybe "" ("&pageToken=" ++) npt
  let url = d ++ nptP
  r <- getWith opts url
  return $ fromResponse r

fromResponse :: Response BS.ByteString -> ([Channel], Maybe PageToken)
fromResponse r = (channels, nextPageToken)
  where
    parseR r' k = T.unpack <$> r' ^.. responseBody . key "items" . values . key "snippet" . k . _String
    channelIds = parseR r (key "resourceId" . key "channelId")
    channelNames = parseR r (key "title")
    channels = zipWith Channel channelIds channelNames
    nextPageToken = T.unpack <$> r ^? responseBody . key "nextPageToken" . _String
