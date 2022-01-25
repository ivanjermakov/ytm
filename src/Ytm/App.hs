{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Ytm.App where

import qualified Brick.AttrMap as A
import Brick.BChan (BChan, newBChan, writeBChan)
import qualified Brick.Main as M
import Brick.Types (Widget)
import qualified Brick.Types as T
import Brick.Util (fg, on)
import Brick.Widgets.Core
import qualified Brick.Widgets.List as L
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Text.Printf (printf)
import Ytm.Api
import Ytm.Api.Channel (subscriptions)
import Ytm.Api.Time (daysBefore)
import Ytm.Api.Video (channelVideos)

data State = State
  { bChan :: BChan CustomEvent,
    sStatus :: String,
    sCredentials :: Maybe Credentials,
    sChannels :: [Channel],
    sLoadedChannels :: Int,
    sVideos :: [Video],
    sVideosL :: L.List () Video
  }

instance Show State where
  show (State _ s c ch lCh v vl) = "State " ++ show (s, c, ch, lCh, v, vl)

data CustomEvent
  = CredentialsLoaded Credentials
  | ChannelsLoaded [Channel]
  | ChannelVideosLoaded [Video]
  | VideosLoaded
  deriving (Show)

runApp :: IO State
runApp = do
  ch <- newBChan 1000
  let s = State ch "application started" Nothing [] 0 [] (L.list () (Vec.fromList []) 1)

  void . forkIO $ do
    c <- credentials
    writeBChan ch $ CredentialsLoaded c

  initialVty <- V.mkVty V.defaultConfig
  M.customMain initialVty (V.mkVty V.defaultConfig) (Just ch) app s

app :: M.App State CustomEvent ()
app = M.App draw chooseCursor handleEvent startEvent attrMap

draw :: State -> [Widget ()]
draw s = [vBox [ls, hl, sl]]
  where
    ls = L.renderList drawListItem True (sVideosL s)
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
    help = withAttr secondaryTextAttr $ str $ intercalate "   " ["(f1) help", "(d) download", "(r) remove"]
    spacer = vLimit 1 $ fill ' '

chooseCursor :: s -> [T.CursorLocation n] -> Maybe (T.CursorLocation n)
chooseCursor = M.showFirstCursor

handleEvent :: State -> T.BrickEvent () CustomEvent -> T.EventM () (T.Next State)
handleEvent s e = case e of
  T.AppEvent cusE -> case cusE of
    (CredentialsLoaded c) -> handleCredentialsLoaded c s
    (ChannelsLoaded chs) -> M.continue (s {sChannels = chs, sStatus = "channels loaded : " ++ show (length chs)})
    (ChannelVideosLoaded vs) -> handleChannelVideosLoaded vs s
    VideosLoaded -> M.continue (s {sVideosL = L.list () (Vec.fromList $ sVideos s) 1})
  T.VtyEvent k -> case k of
    V.EvKey (V.KChar 'q') [] -> M.halt s
    V.EvKey (V.KChar 'i') [] -> moveBy (-1)
    V.EvKey (V.KChar 'k') [] -> moveBy 1
    _ -> handleL
    where
      moveBy n = M.continue . (\l -> s {sVideosL = l}) . L.listMoveBy n =<< L.handleListEvent k (sVideosL s)
      handleL = M.continue . (\l -> s {sVideosL = l}) =<< L.handleListEvent k (sVideosL s)

handleCredentialsLoaded :: Credentials -> State -> T.EventM () (T.Next State)
handleCredentialsLoaded c s = do
  let ns = (s {sCredentials = Just c, sStatus = "loading channels"})
  void . liftIO . forkIO $ do
    chs <- subscriptions c
    writeBChan (bChan s) (ChannelsLoaded chs)
    void $ mapConcurrently chLoaded chs
    writeBChan (bChan s) VideosLoaded
  M.continue ns
  where
    chLoaded ch = do
      db <- daysBefore 4
      cVs <- channelVideos db ch c
      writeBChan (bChan s) (ChannelVideosLoaded cVs)

handleChannelVideosLoaded :: [Video] -> State -> T.EventM () (T.Next State)
handleChannelVideosLoaded vs s =
  M.continue
    ( s
        { sVideos = sVideos s ++ vs,
          sLoadedChannels = sLoadedChannels s + 1,
          sStatus = "loading videos from channels: " ++ countStr
        }
    )
  where
    countStr =
      printf
        "%d/%d (%d)"
        (1 + sLoadedChannels s)
        (length (sChannels s))
        (length (sVideos s))

startEvent :: State -> T.EventM () State
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
