{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Ytm.App where

import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Main as M
import qualified Brick.Types as T
import Control.Concurrent (forkIO)
import Control.Monad (void)
import qualified Graphics.Vty as V
import Ytm.Api
import Ytm.App.Attr (attrMap)
import Ytm.App.Draw
import Ytm.App.State
import Ytm.App.Types

runApp :: IO State
runApp = do
  ch <- newBChan 1000
  let s = initState ch

  void . forkIO $ do
    c <- credentials
    writeBChan ch $ CredentialsLoaded c

  initialVty <- V.mkVty V.defaultConfig
  M.customMain initialVty (V.mkVty V.defaultConfig) (Just ch) app s

app :: M.App State CustomEvent ()
app = M.App draw chooseCursor handleEvent startEvent attrMap

chooseCursor :: s -> [T.CursorLocation n] -> Maybe (T.CursorLocation n)
chooseCursor = M.showFirstCursor

startEvent :: State -> T.EventM () State
startEvent = return
