{-# LANGUAGE BlockArguments #-}

module Ytm.App where

import Brick.BChan (newBChan)
import qualified Brick.Main as M
import qualified Brick.Types as T
import Control.Concurrent.Async (async)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Graphics.Vty as V
import Ytm.Api
import Ytm.App.Attr (attrMap)
import Ytm.App.Draw
import Ytm.App.State
import Ytm.App.State.Core
import Ytm.App.Types

runApp :: Settings -> IO State
runApp settings = do
  ch <- newBChan 1000
  let s = initState ch

  liftIO . async $ do
    sendChan (SettingsLoaded settings) s

  c <- V.userConfig
  initialVty <- V.mkVty c
  M.customMain initialVty (V.mkVty V.defaultConfig) (Just ch) app s

app :: M.App State CustomEvent ResourceName
app = M.App draw chooseCursor handleEvent startEvent attrMap

chooseCursor :: s -> [T.CursorLocation n] -> Maybe (T.CursorLocation n)
chooseCursor = M.showFirstCursor

startEvent :: State -> T.EventM ResourceName State
startEvent = return
