{-# LANGUAGE TupleSections #-}

module Ytm.App.Draw where

import Brick.Types
import Brick.Widgets.Center (center)
import Brick.Widgets.Core
import qualified Brick.Widgets.List as L
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import Ytm.Api
import Ytm.Api.Time
import Ytm.App.Attr
import Ytm.App.Types

draw :: State -> [Widget ResourceName]
draw s = [vBox [main, hl, sl]]
  where
    main = if null . sVideos $ s then noVs else ls
    ls = L.renderList drawListItem True (fmap (,sVideosLWidth s) . sVideosL $ s)
    noVs = center $ str "no videos loaded"
    hl = drawHelpLine s
    sl = drawStatusLine s

drawListItem :: Bool -> (Video, Int) -> Widget ResourceName
drawListItem _ (v, w) = hBox [vTitle, chName, pubDate]
  where
    (ts : ns : ds : _) = toFractions [10, 4, 2] w
    vTitle = toSize ts . str . videoTitle $ v
    chName = toSize ns . str . channelName $ channel v
    pubDate = toSize ds . str . showUTCTime "%R %b %d" . publishedAt $ v

drawStatusLine :: State -> Widget ResourceName
drawStatusLine s = hBox [str (sStatus s), hSpacer, str position, str (show $ sVideosLWidth s)]
  where
    current = (+ 1) . fromMaybe (-1) . L.listSelected . sVideosL $ s
    total = length . sVideosL $ s
    position = printf "%d/%d" current total

drawHelpLine :: State -> Widget ResourceName
drawHelpLine _ = hBox [help, hSpacer]
  where
    help = withAttr secondaryTextAttr $ str $ intercalate "   " ["(f1) help", "(r) refresh", "(d) download", "(r) remove"]

hSpacer :: Widget ResourceName
hSpacer = vLimit 1 $ fill ' '

toSize :: Int -> Widget ResourceName -> Widget ResourceName
toSize n = hLimit n . padRight Max

toFractions :: [Int] -> Int -> [Int]
toFractions frs w = init res ++ [last res + offset]
  where
    offset = w - sum res
    res = map calc frs
    tFrs = sum frs
    calc fr = floor $ fromIntegral fr / fromIntegral tFrs * fromIntegral w
