{-# LANGUAGE OverloadedStrings #-}

module Ytm.App.Attr where

import qualified Brick.AttrMap as A
import Brick.Util
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as V

attrMap :: s -> A.AttrMap
attrMap _ =
  A.attrMap
    V.defAttr
    [ (L.listSelectedAttr, V.black `on` V.brightWhite),
      (secondaryTextAttr, fg V.brightBlack)
    ]

secondaryTextAttr :: A.AttrName
secondaryTextAttr = "secondaryText"
