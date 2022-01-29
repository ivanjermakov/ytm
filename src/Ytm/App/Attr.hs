{-# LANGUAGE OverloadedStrings #-}

module Ytm.App.Attr where

import qualified Brick.AttrMap as A
import Brick.Util
import qualified Graphics.Vty as V

-- TODO: themes
attrMap :: s -> A.AttrMap
attrMap _ =
  A.attrMap
    V.defAttr
    [ (itemAttr, V.defAttr),
      (itemSelAttr, V.black `on` V.white),
      (secondaryTextAttr, fg V.brightMagenta),
      (downloadingItemAttr, fg V.yellow),
      (downloadingItemSelAttr, bg V.yellow),
      (downloadedItemAttr, fg V.green),
      (downloadedItemSelAttr, bg V.green)
    ]

secondaryTextAttr :: A.AttrName
secondaryTextAttr = "secondaryText"

itemAttr :: A.AttrName
itemAttr = "item"

itemSelAttr :: A.AttrName
itemSelAttr = "itemSel"

downloadingItemAttr :: A.AttrName
downloadingItemAttr = "downloadingItem"

downloadingItemSelAttr :: A.AttrName
downloadingItemSelAttr = "downloadingItemSel"

downloadedItemAttr :: A.AttrName
downloadedItemAttr = "downloadedItem"

downloadedItemSelAttr :: A.AttrName
downloadedItemSelAttr = "downloadedItemSel"
