module Day9Spec where

import Test.Hspec
import Day9

spec :: Spec
spec = do
  describe "parseString" $ do
    it "parses garbage from string 1" $ do
      let string = "<random characters>"
      parseString string `shouldBe` Right (Garbage "random characters")

    it "parses garbage from string 2" $ do
      let string = "<<<<>"
      parseString string `shouldBe` Right (Garbage "<<<")

    it "parses garbage from string 3" $ do
      let string = "<{o\"i!a,<{i<a>"
      parseString string `shouldBe` Right (Garbage "{o\"i,<{i<a")

    it "parses groups from string 1" $ do
      let string = "{{{}}}"
      parseString string `shouldBe` Right (Group [Group [Group []]])

    it "parses groups from string 2" $ do
      let string = "{<a>,<a>,<a>}"
      parseString string `shouldBe` Right (Group [Garbage "a", Garbage "a", Garbage "a"])

  describe "score" $ do
    it "calculates score of a group 1" $ do
      let group = Group []
      score group `shouldBe` 1

    it "calculates score of a group 2" $ do
      let group = Group [Group [Group []]]
      score group `shouldBe` 6

    it "calculates score of a group 3" $ do
      let group = Group [Group [Group [], Group [], Group [Group []]]]
      score group `shouldBe` 16

    it "calculates score of a group 4" $ do
      let group = Group [Garbage "a", Garbage "a", Garbage "a"]
      score group `shouldBe` 1

    file <- runIO $ readFile "test/day9.txt"
    it "solves the puzzle" $ do
      let group = case (parseString file)
                  of Right s -> s
                     _ -> Group []
      score group `shouldBe` 23588

  describe "countGarbage" $ do
    it "counts garbage of a group 1" $ do
      let group = Garbage ""
      countGarbage group `shouldBe` 0

    it "counts garbage of a group 2" $ do
      let group = Garbage "random characters"
      countGarbage group `shouldBe` 17

    it "counts garbage of a group 3" $ do
      let group = Garbage "<<<"
      countGarbage group `shouldBe` 3

    it "counts garbage of a group 4" $ do
      let group = Garbage "{}"
      countGarbage group `shouldBe` 2

    it "counts garbage of a group 5" $ do
      let group = Garbage "{o\"i,<{i<a"
      countGarbage group `shouldBe` 10

    it "counts garbage of a group 6" $ do
      let group = Group [Garbage "a", Garbage "a", Garbage "a"]
      countGarbage group `shouldBe` 3

    it "counts garbage of a group 7" $ do
      let group = Group [Group [Group [Garbage "foo"], Group [Garbage "bar"], Group [Group [Garbage "baz"]]]]
      countGarbage group `shouldBe` 9

    file <- runIO $ readFile "test/day9.txt"
    it "solves the puzzle" $ do
      let group = case (parseString file)
                  of Right s -> s
                     _ -> Group []
      countGarbage group `shouldBe` 10045
