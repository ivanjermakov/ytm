module Ytm.App.Draw where

import Brick.Types
import qualified Brick.Types as T
import Brick.Widgets.Center (center)
import Brick.Widgets.Core
import qualified Brick.Widgets.List as L
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import Ytm.Api
import Ytm.App.Attr
import Ytm.App.Types

draw :: State -> [Widget ()]
draw s = [vBox [main, hl, sl]]
  where
    main = if null . sVideos $ s then noVs else ls
    ls = L.renderList drawListItem True (sVideosL s)
    noVs = center $ str "no videos loaded"
    hl = drawHelpLine s
    sl = drawStatusLine s

drawListItem :: Bool -> Video -> Widget ()
drawListItem _ v = hBox [padRight T.Max textW]
  where
    textW = str (channelName (channel v) ++ videoTitle v)

drawStatusLine :: State -> Widget ()
drawStatusLine s = hBox [str (sStatus s), spacer, str position]
  where
    current = (+ 1) . fromMaybe (-1) . L.listSelected . sVideosL $ s
    total = length . sVideosL $ s
    position = printf "%d/%d" current total
    spacer = vLimit 1 $ fill ' '

drawHelpLine :: State -> Widget ()
drawHelpLine _ = hBox [help, spacer]
  where
    help = withAttr secondaryTextAttr $ str $ intercalate "   " ["(f1) help", "(r) refresh", "(d) download", "(r) remove"]
    spacer = vLimit 1 $ fill ' '
