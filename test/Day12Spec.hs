module Day12Spec where

import Test.Hspec
import Day12

spec :: Spec
spec = do
  describe "connections from string" $ do
    it "reads connections from string 1" $ do
      readConnections "" `shouldBe` []

    it "reads connections from string 2" $ do
      readConnections "4 <-> 2, 3, 6" `shouldBe` [("4", "2"), ("4", "3"), ("4", "6")]
