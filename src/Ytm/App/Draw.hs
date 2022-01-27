{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module Ytm.App.Draw where

import Brick.Types
import Brick.Widgets.Center (center)
import Brick.Widgets.Core
import qualified Brick.Widgets.List as L
import Data.List (intercalate, intersperse)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import Ytm.Api
import Ytm.Api.Time
import Ytm.App.Attr
import Ytm.App.Types

draw :: State -> [Widget ResourceName]
draw s = [vBox [header, hBox [vBox [main, hl, sl]]]]
  where
    w = sVideosLWidth s
    header = drawListHeader w
    main = if null . sVideos $ s then noVs else ls
    noVs = center $ str "no videos loaded"
    ls = L.renderList drawListItem True (fmap (,w) . sVideosL $ s)
    hl = drawHelpLine s
    sl = drawStatusLine s

listRatios :: [Float]
listRatios = [1, 10, 4, 2]

drawListHeader :: Int -> Widget ResourceName
drawListHeader w = hBoxGapped 1 [strFixed ps "s", strFixed ts "title", strFixed ns "channel", strFixed ds "date"]
  where
    (ps : ts : ns : ds : _) = toFractions listRatios (w - 1)

drawListItem :: Bool -> (VideoItem, Int) -> Widget ResourceName
drawListItem _ (i, w) = hBoxGapped 1 [progress, vTitle, chName, pubDate]
  where
    (ps : ts : ns : ds : _) = toFractions listRatios (w - 1)
    v = itemVideo i
    vTitle = strFixed ts . videoTitle $ v
    chName = strFixed ns . channelName $ channel v
    pubDate = strFixed ds . showUTCTime "%R %b %d" . publishedAt $ v
    progress = strFixed ps case itemStatus i of
      Available -> ""
      Downloaded -> "D"
      Downloading -> case itemProgress i of
        Nothing -> "D~~"
        Just p -> printf "%03.0f%%" p

drawStatusLine :: State -> Widget ResourceName
drawStatusLine s = hBox [str (sStatus s), hSpacer, str position]
  where
    current = (+ 1) . fromMaybe (-1) . L.listSelected . sVideosL $ s
    total = length . sVideosL $ s
    position = printf "%d/%d" current total

drawHelpLine :: State -> Widget ResourceName
drawHelpLine _ = hBox [help, hSpacer]
  where
    help = withAttr secondaryTextAttr $ str $ intercalate "   " ["(f1) help", "(r) refresh", "(d) download", "(x) remove"]

hSpacer :: Widget ResourceName
hSpacer = vLimit 1 $ fill ' '

padStrRight :: Int -> String -> String
padStrRight n s = s ++ replicate (n - length s) ' '

strFixed :: Int -> String -> Widget ResourceName
strFixed n s = toSize n (str . padStrRight n $ s)

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
