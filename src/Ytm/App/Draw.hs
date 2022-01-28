{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module Ytm.App.Draw where

import Brick.Types
import Brick.Widgets.Center (center)
import Brick.Widgets.Core
import qualified Brick.Widgets.List as L
import Data.List (intercalate, intersperse)
import Text.Printf (printf)
import Ytm.Api
import Ytm.App.Attr
import Ytm.App.State.Core
import Ytm.App.Types
import Ytm.Util.Range (inRange)
import Ytm.Util.Time

draw :: State -> [Widget ResourceName]
draw s = [vBox [main, hSpacer, hl, sl]]
  where
    header = drawListHeader . sVideosLWidth $ s
    main = if null . sVideos $ s then noVs else ls
    noVs = center $ str "no videos loaded"
    ls = vBox [header, L.renderListWithIndex drawListItem True (fmap (,s) . sVideosL $ s)]
    hl = drawHelpLine s
    sl = drawStatusLine s

listRatios :: [Float]
listRatios = [0.5, 1.5, 10, 4, 2]

drawListHeader :: Int -> Widget ResourceName
drawListHeader w =
  hBoxGapped 1 $
    map
      (uncurry (withAttr secondaryTextAttr .))
      [ (strFixedRight ps, "s"),
        (strFixedLeft dus, "duration"),
        (strFixedRight ts, "title"),
        (strFixedRight ns, "channel"),
        (strFixedRight ds, "date")
      ]
  where
    (ps : dus : ts : ns : ds : _) = toFractions listRatios (w - 4)

drawListItem :: Int -> Bool -> (VideoItem, State) -> Widget ResourceName
drawListItem ix _ (i, s) = applySelAttr itemBox
  where
    applySelAttr = if isSel then withAttr L.listSelectedAttr else id
    itemBox = hBoxGapped 1 [progress, dur, vTitle, chName, pubDate]
    isSel = case sSelectMode s of
      Nothing -> False
      Just sm -> inRange ix sm
    w = sVideosLWidth s
    (ps : dus : ts : ns : ds : _) = toFractions listRatios (w - 4)
    v = itemVideo i
    dur = strFixedLeft dus . showTime "%_mm:%0Ss" . videoDuration $ v
    vTitle = strFixedRight ts . videoTitle $ v
    chName = strFixedRight ns . channelName $ channel v
    pubDate = strFixedRight ds . showTime "%R %b %d" . publishedAt $ v
    progress = strFixedRight ps case itemStatus i of
      Available -> ""
      Downloaded -> "D"
      Downloading -> case itemProgress i of
        Nothing -> "D~~"
        Just p -> printf "%.0f%%" p

-- TODO: downloading videos stats
-- TODO: network usage indicator
drawStatusLine :: State -> Widget ResourceName
drawStatusLine s = hBox [str (sStatus s), hSpacer, hBoxGapped 1 [str sMode, str vId, str position]]
  where
    mVi = activeVideoItem s
    current = maybe 0 (+ 1) . L.listSelected . sVideosL $ s
    total = length . sVideosL $ s
    vId = maybe "" (videoId . itemVideo) mVi
    position = printf "%d/%d" current total
    sMode = show . sSelectMode $s

drawHelpLine :: State -> Widget ResourceName
drawHelpLine _ = hBox [help, hSpacer]
  where
    help = withAttr secondaryTextAttr $ str $ intercalate "   " ["(r) refresh", "(d) download", "(x) remove", "(v) select"]

hSpacer :: Widget ResourceName
hSpacer = vLimit 1 $ fill ' '

padStrRight :: Int -> String -> String
padStrRight n s = s ++ replicate (n - length s) ' '

strFixedRight :: Int -> String -> Widget ResourceName
strFixedRight n s = toSize n (str . padStrRight n $ s)

padStrLeft :: Int -> String -> String
padStrLeft n s = replicate (n - length s) ' ' ++ s

strFixedLeft :: Int -> String -> Widget ResourceName
strFixedLeft n s = toSize n (str . padStrLeft n $ s)

toSize :: Int -> Widget ResourceName -> Widget ResourceName
toSize n = hLimit n . padRight Max

hBoxGapped :: Int -> [Widget ResourceName] -> Widget ResourceName
hBoxGapped n ws = hBox $ intersperse (str . replicate n $ ' ') ws

-- TODO: support fixed sized
toFractions :: [Float] -> Int -> [Int]
toFractions frs w = init res ++ [last res + offset]
  where
    offset = w - sum res
    res = map calc frs
    tFrs = sum frs
    calc fr = floor $ fr / tFrs * fromIntegral w
