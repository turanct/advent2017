module Day7Spec where

import Test.Hspec
import Day7

spec :: Spec
spec = do
  describe "readProgram" $ do
    it "reads a simple program without children" $ do
      readProgram "foo (66)" `shouldBe` Program "foo" 66 []

    it "reads a simple program with one child" $ do
      readProgram "foo (66) -> bar" `shouldBe` Program "foo" 66 ["bar"]

    it "reads a simple program with children" $ do
      readProgram "foo (66) -> bar, baz, qux" `shouldBe` Program "foo" 66 ["bar", "baz", "qux"]
