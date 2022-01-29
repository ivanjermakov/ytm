module Ytm.App.Draw.FlexSpec where

import Test.Hspec
import Ytm.App.Draw.Flex

spec :: Spec
spec = do
  describe "calculateChildrenSizes" $ do
    it "all fixed" $ do
      shouldBe
        (calculateChildrenSizes [F 10, F 20, F 30] 100)
        [10, 20, 30]
    it "fixed with maxC" $ do
      shouldBe
        (calculateChildrenSizes [F 10, MaxContent, F 30] 100)
        [10, 60, 30]
    it "fixed + relative" $ do
      shouldBe
        (calculateChildrenSizes [F 10, R 1, R 2] 100)
        [10, 30, 60]
    it "fixed + relative + maxC" $ do
      shouldBe
        (calculateChildrenSizes [F 10, R 1, R 1, MaxContent] 100)
        [10, 30, 30, 30]
    it "sum to full" $ do
      shouldBe
        (sum $ calculateChildrenSizes [F 10, R 2, R 3, MaxContent] 100)
        100
      shouldBe
        (sum $ calculateChildrenSizes [R 1, R 2, R 3, MaxContent] 498)
        498
