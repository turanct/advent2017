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

  describe "direct connections" $ do
    let cs = [ ("0", "2"), ("1", "1"), ("2", "0"), ("2", "3"), ("2", "4")
             , ("3", "2"), ("3", "4"), ("4", "2"), ("4", "3"), ("4", "6")
             , ("5", "6"), ("6", "4"), ("6", "5") ]

    it "finds direct connections 1" $ do
      directConnections cs "9" `shouldBe` []

    it "finds direct connections 2" $ do
      directConnections cs "1" `shouldBe` ["1"]

    it "finds direct connections 3" $ do
      directConnections cs "2" `shouldBe` ["0", "3", "4"]

  describe "find group" $ do
    let cs = [ ("0", "2"), ("1", "1"), ("2", "0"), ("2", "3"), ("2", "4")
             , ("3", "2"), ("3", "4"), ("4", "2"), ("4", "3"), ("4", "6")
             , ("5", "6"), ("6", "4"), ("6", "5") ]

    it "find the group with one member" $ do
      findGroup cs "1" `shouldBe` ["1"]

    it "finds the complete group of a group member" $ do
      findGroup cs "0" `shouldBe` ["2","0","3","4","6","5"]

  describe "part 1" $ do
    file <- runIO $ readFile "test/day12.txt"

    it "solves the puzzle" $ do
      let cs = concat $ map readConnections $ lines file
      part1 cs "0" `shouldBe` 239

  describe "find groups" $ do
    let cs = [ ("0", "2"), ("1", "1"), ("2", "0"), ("2", "3"), ("2", "4")
             , ("3", "2"), ("3", "4"), ("4", "2"), ("4", "3"), ("4", "6")
             , ("5", "6"), ("6", "4"), ("6", "5") ]

    it "finds all groups" $ do
      findGroups cs `shouldBe` [["1"], ["2","0","3","4","6","5"]]

  describe "part 2" $ do
    file <- runIO $ readFile "test/day12.txt"

    it "solves the puzzle" $ do
      let cs = concat $ map readConnections $ lines file
      part2 cs `shouldBe` 215
