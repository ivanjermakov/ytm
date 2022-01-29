module Ytm.App.Draw.Flex where

import Brick.Types
import Brick.Widgets.Core
import qualified Graphics.Vty as V

data Fr = F Int | R Float | MaxContent

calculateChildrenSizes :: [Fr] -> Int -> [Int]
calculateChildrenSizes frs w =
  case w - sum rawRes of
    0 -> rawRes
    x ->
      if allFixed
        then rawRes
        else zipWith (curry (appendLostCol x)) frs rawRes
  where
    fixedW = sum [x | (F x) <- frs]
    allFixed = sum [1 | x@(F _) <- frs] == length frs
    totalR = sum [r | (R r) <- frs] + fromIntegral (length [m | m@MaxContent <- frs])
    relativeW = fromIntegral $ w - fixedW
    f fr = case fr of
      F x -> x
      R r -> floor $ (r / totalR) * relativeW
      MaxContent ->
        if totalR == 0
          then floor relativeW
          else floor $ (1 / totalR) * relativeW
    rawRes = map f frs
    appendLostCol x (MaxContent, w') = w' + x
    appendLostCol _ (_, res) = res

flexHGap :: Int -> ([Widget n] -> Widget n) -> [(Fr, Widget n)] -> Int -> Widget n
flexHGap gap composer frws pw = Widget Greedy Fixed renderFlexBoxH
  where
    (frs, ws) = unzip frws
    renderFlexBoxH = do
      render
        . composer
        . zipWith (flip $ toSize gap) ws
        . calculateChildrenSizes frs
        $ pw - ((length frs - 1) * gap)

flexH :: ([Widget n] -> Widget n) -> [(Fr, Widget n)] -> Int -> Widget n
flexH = flexHGap 0

widgetDimensions :: Widget n -> RenderM n (Int, Int)
widgetDimensions w = do
  res <- render w
  let i = image res
  return (V.imageWidth i, V.imageHeight i)

toSize :: Int -> Int -> Widget n -> Widget n
toSize g n w = hBox [hLimit n . padRight Max $ w, str . replicate g $ ' ']
