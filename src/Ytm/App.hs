{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Ytm.App where

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import Brick.Types (Widget)
import qualified Brick.Types as T
import Brick.Util (fg, on)
import Brick.Widgets.Core
import qualified Brick.Widgets.List as L
import Data.List (intercalate)
import Data.Maybe (fromJust)
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Text.Printf (printf)

data State = State {list :: L.List () String}

runApp :: IO State
runApp = M.defaultMain app $ State (L.list () (Vec.fromList ["some", "stuff", "here"]) 1)

app :: M.App State e ()
app = M.App draw chooseCursor handleEvent startEvent attrMap

draw :: State -> [Widget ()]
draw s = [vBox [ls, hl, sl]]
  where
    ls = L.renderList drawListItem True (list s)
    hl = drawHelpLine s
    sl = drawStatusLine s

drawListItem :: Bool -> String -> Widget ()
drawListItem _ s = hBox [padRight T.Max textW]
  where
    textW = str s

drawStatusLine :: State -> Widget ()
drawStatusLine State {list = l} = hBox [str "status line here", spacer, str position]
  where
    current = (+ 1) . fromJust . L.listSelected $ l
    total = length l
    position = printf "%d/%d" current total
    spacer = vLimit 1 $ fill ' '

drawHelpLine :: State -> Widget ()
drawHelpLine _ = hBox [help, spacer]
  where
    help = withAttr secondaryTextAttr $ str $ intercalate "   " ["(f1) help", "(d) download", "(r) remove"]
    spacer = vLimit 1 $ fill ' '

chooseCursor :: s -> [T.CursorLocation n] -> Maybe (T.CursorLocation n)
chooseCursor = M.showFirstCursor

handleEvent :: State -> T.BrickEvent () e -> T.EventM () (T.Next State)
handleEvent s (T.VtyEvent e) = case e of
  V.EvKey (V.KChar 'q') [] -> M.halt s
  V.EvKey (V.KChar 'i') [] -> moveBy (-1)
  V.EvKey (V.KChar 'k') [] -> moveBy (1)
  _ -> M.continue . (\l -> s {list = l}) =<< L.handleListEvent e (list s)
  where
    moveBy n = M.continue . (\l -> s {list = l}) . L.listMoveBy n =<< L.handleListEvent e (list s)

startEvent :: s -> T.EventM n s
startEvent = return

attrMap :: s -> A.AttrMap
attrMap _ =
  A.attrMap
    V.defAttr
    [ (L.listSelectedAttr, V.black `on` V.brightWhite),
      (secondaryTextAttr, fg V.brightBlack)
    ]

secondaryTextAttr :: A.AttrName
secondaryTextAttr = "secondaryText"
