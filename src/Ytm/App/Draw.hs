{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module Ytm.App.Draw where

import Brick.Types
import Brick.Widgets.Center (center)
import Brick.Widgets.Core
import qualified Brick.Widgets.List as L
import Data.List (intercalate)
import Text.Printf (printf)
import Ytm.Api
import Ytm.App.Attr
import Ytm.App.Draw.Flex
import Ytm.App.State.Core
import Ytm.App.Types
import Ytm.Util.Range (inRange)
import Ytm.Util.Time

draw :: State -> [Widget ResourceName]
draw s = [vBox [main, hSpacer, hl, sl]]
  where
    header = drawListHeader . sVideosLWidth $ s
    main = if null . sVideos $ s then noVs else ls
    noVs = center . withAttr secondaryTextAttr . str $ "no videos loaded"
    ls = vBox [header, L.renderListWithIndex drawListItem True (fmap (,s) . sVideosL $ s)]
    hl = drawHelpLine s
    sl = drawStatusLine s

listRatios :: [Fr]
listRatios = [F 4, F 8, R 3, R 1, F 12]

drawListHeader :: Int -> Widget ResourceName
drawListHeader =
  flexHGap 1 hBox
    . zip listRatios
    . map (withAttr secondaryTextAttr . str)
    $ ["s", "duration", "title", "channel", "date"]

drawListItem :: Int -> Bool -> (VideoItem, State) -> Widget ResourceName
drawListItem ix isActive (i, s) = applyAttr itemBox
  where
    applyAttr = withAttr case (isSel || isActive, itemStatus i) of
      (True, Downloading) -> downloadingItemSelAttr
      (False, Downloading) -> downloadingItemAttr
      (True, Downloaded) -> downloadedItemSelAttr
      (False, Downloaded) -> downloadedItemAttr
      (True, Available) -> itemSelAttr
      (False, Available) -> itemAttr
    itemBox =
      flexHGap
        1
        hBox
        (zip listRatios (map str [progress, dur, videoTitle v, chName, pubDate]))
        (sVideosLWidth s)
    isSel = case sSelectMode s of
      Nothing -> False
      Just sm -> inRange ix sm
    v = itemVideo i
    dur = showTime "%_mm:%0Ss" . videoDuration $ v
    chName = channelName $ channel v
    pubDate = showTime "%R %b %d" . publishedAt $ v
    progress = case itemStatus i of
      Available -> " "
      Downloaded -> "D"
      Downloading -> case itemProgress i of
        Nothing -> "D~~"
        Just p -> if downPercent p == 100 then "100%" else printf "%.0f%%" (downPercent p)

-- TODO: downloading videos stats
-- TODO: network usage indicator
drawStatusLine :: State -> Widget ResourceName
drawStatusLine s =
  hBox
    [ str (sStatus s),
      hSpacer,
      hBox [padRight (Pad 1) . str $ vId, str position]
    ]
  where
    mVi = activeVideoItem s
    current = maybe 0 (+ 1) . L.listSelected . sVideosL $ s
    total = length . sVideosL $ s
    vId = maybe "" (videoId . itemVideo) mVi
    position = printf "%d/%d" current total

drawHelpLine :: State -> Widget ResourceName
drawHelpLine s = hBox [help, hSpacer]
  where
    help = withAttr secondaryTextAttr $ str $ intercalate "   " hs
    hs = case sSelectMode s of
      Nothing -> case activeVideoItem s of
        Just i -> case itemStatus i of
          Available -> [r, d, v, p]
          Downloading -> [r, v]
          Downloaded -> [r, x, v, p]
        Nothing -> [r]
      Just _ -> [r, d, x, v]
    (r : d : x : v : p : _) = ["(r) refresh", "(d) download", "(x) remove", "(v) select", "(Enter) play"]

hSpacer :: Widget ResourceName
hSpacer = vLimit 1 $ fill ' '

padStrRight :: Int -> String -> String
padStrRight n s = s ++ replicate (n - length s) ' '

strFixedRight :: Int -> String -> Widget ResourceName
strFixedRight n s = toSize 0 n (str . padStrRight n $ s)

padStrLeft :: Int -> String -> String
padStrLeft n s = replicate (n - length s) ' ' ++ s

strFixedLeft :: Int -> String -> Widget ResourceName
strFixedLeft n s = toSize 0 n (str . padStrLeft n $ s)
