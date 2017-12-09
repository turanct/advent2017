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

  describe "childOf" $ do
    it "returns False if the given Program is not a child of the other" $ do
      let foo = Program "foo" 10 []
      let bar = Program "bar" 20 []
      (foo `childOf` bar) `shouldBe` False

    it "returns True if the given Program is a child of the other" $ do
      let foo = Program "foo" 10 []
      let bar = Program "bar" 20 ["baz", "foo", "qux"]
      (foo `childOf` bar) `shouldBe` True

  describe "topProgram" $ do
    it "finds the topmost program out of a list of programs" $ do
      let programs = [ Program "foo" 10 ["bar", "baz"]
                     , Program "bar" 20 []
                     , Program "baz" 30 []
                     , Program "qux" 40 ["foo"] ]
      topProgram programs `shouldBe` Program "qux" 40 ["foo"]

    file <- runIO $ readFile "test/day7.txt"

    it "solves the puzzle" $ do
      let programs = map readProgram $ lines file
      topProgram programs `shouldBe` Program "wiapj" 55 ["djzjiwd","lsire","vlbivgc","xdctkbj","ygvpk"]

  describe "programByName" $ do
    it "finds a program by name" $ do
      let programs = [ Program "foo" 10 ["bar", "baz"]
                     , Program "bar" 20 []
                     , Program "baz" 30 []
                     , Program "qux" 40 ["foo"] ]
      programByName programs "bar" `shouldBe` Program "bar" 20 []

  describe "weight" $ do
    it "calculates the weight of a program and its children" $ do
      let programs = [ Program "foo" 10 ["bar", "baz"]
                     , Program "bar" 20 []
                     , Program "baz" 30 []
                     , Program "qux" 40 ["foo"] ]
      weight programs "foo" `shouldBe` 60

  describe "balanced" $ do
    it "returns True if all children have the same weight" $ do
      let programs = [ Program "foo" 10 ["bar", "baz"]
                     , Program "bar" 20 []
                     , Program "baz" 20 []
                     , Program "qux" 40 ["foo"] ]
      balanced programs "foo" `shouldBe` True

    it "returns False if not all children have the same weight" $ do
      let programs = [ Program "foo" 10 ["bar", "baz"]
                     , Program "bar" 20 []
                     , Program "baz" 30 []
                     , Program "qux" 40 ["foo"] ]
      balanced programs "foo" `shouldBe` False

    it "returns True if the program has no children" $ do
      let programs = [ Program "foo" 10 ["bar", "baz"]
                     , Program "bar" 20 []
                     , Program "baz" 30 []
                     , Program "qux" 40 ["foo"] ]
      balanced programs "baz" `shouldBe` True

  describe "unbalanced" $ do
    file <- runIO $ readFile "test/day7.txt"

    it "finds balanced programs" $ do
      let programs = map readProgram $ lines file
      let unbalanced = filter (\(Program n _ _ ) -> not $ balanced programs n) programs
      let childrenWeight = map (weight programs) ["izdhn","yzhvrx","eionkb","eadvs","jkkqxfr"]
      childrenWeight `shouldBe` [1777,1777,1784,1777,1777]
      -- i cheated a bit here because i got fed up. with the above i could manually calculate
