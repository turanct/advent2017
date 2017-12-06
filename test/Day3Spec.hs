module Day3Spec where

import Test.Hspec
import Day3

spec :: Spec
spec = do
  describe "step1" $ do
    it "calculates for spiral 2" $ do
      steps1 2 `shouldBe` 1

    it "calculates for spiral 3" $ do
      steps1 12 `shouldBe` 3

    it "calculates for futher down the spiral" $ do
      steps1 36 `shouldBe` 5

    it "works for a high number" $ do
      steps1 1024 `shouldBe` 31

    it "solves the puzzle" $ do
      steps1 265149 `shouldBe` 438
