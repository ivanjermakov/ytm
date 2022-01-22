{-# LANGUAGE OverloadedStrings #-}

module Ytm.Api.Channel where

import Control.Lens
import Data.Aeson.Lens
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

-- no nextPageToken means first page
subscriptionsPage :: Maybe PageToken -> Credentials -> IO ([Channel], Maybe PageToken)
subscriptionsPage npt c = do
  let opts = defaults & authorizationHeader c & acceptJsonHeader
  let d = domain ++ "subscriptions"
  let qP = "?part=snippet%2CcontentDetails&maxResults=50&mine=true&key=" ++ clientId c
  let nptP = maybe "" (\t -> "&pageToken=" ++ T.unpack t) npt
  let url = concat [d, qP, nptP]
  r <- getWith opts url
  let channelIds = parseR r (key "resourceId" . key "channelId")
  let channelNames = parseR r (key "title")
  let channels = zipWith Channel channelIds channelNames
  let nextPageToken = r ^? responseBody . key "nextPageToken" . _String
  return (channels, nextPageToken)
  where
    parseR r' k = T.unpack <$> r' ^.. responseBody . key "items" . values . key "snippet" . k . _String
