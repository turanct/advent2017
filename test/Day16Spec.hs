module Day16Spec where

import Test.Hspec
import Day16

spec :: Spec
spec = do
  describe "spin" $ do
    it "doesn't spin if the amount is zero" $ do
      let line = ["a", "b", "c", "d", "e"]
      spin 0 line `shouldBe` line

    it "spins around the line a number of positions" $ do
      let line = ["a", "b", "c", "d", "e"]
      spin 1 line `shouldBe` ["e", "a", "b", "c", "d"]

  describe "exchange" $ do
    it "swaps two positions 1" $ do
      let line = ["e", "a", "b", "c", "d"]
      exchange 3 4 line `shouldBe` ["e", "a", "b", "d", "c"]

    it "swaps two positions 2" $ do
      let line = ["e", "a", "b", "c", "d"]
      exchange 4 3 line `shouldBe` ["e", "a", "b", "d", "c"]

    it "swaps two positions 3" $ do
      let line = ["e", "a", "b", "c", "d"]
      exchange 0 4 line `shouldBe` ["d", "a", "b", "c", "e"]

  describe "partner" $ do
    it "swaps to names 1" $ do
      let line = ["e", "a", "b", "c", "d"]
      partner "c" "d" line `shouldBe` ["e", "a", "b", "d", "c"]

  describe "moves from string" $ do
    it "parses an empty string" $ do
      let string = ""
      movesFromString string `shouldBe` Right []

    it "parses a simple string" $ do
      let string = "x15/1,s15,s15,pa/m"
      movesFromString string `shouldBe` Right [ Exchange 15 1
                                              , Spin 15
                                              , Spin 15
                                              , Partner "a" "m" ]

  describe "dance" $ do
    it "runs a line of programs through multiple dance moves" $ do
      let line = ["a", "b", "c", "d", "e"]
      let moves = [Spin 1, Exchange 3 4, Partner "e" "b"]

      dance line moves `shouldBe` ["b", "a", "e", "d", "c"]

    file <- runIO $ readFile "test/day16.txt"
    it "solves the puzzle" $ do
      let moves = case (movesFromString file)
                  of Right ms -> ms
                     _ -> []
      let line = ["a", "b", "c", "d", "e", "f", "g", "h"
                 ,"i","j", "k", "l", "m", "n", "o", "p"]
      dance line moves `shouldBe` ["c","e","i","j","b","f","o","a"
                                  ,"m","g","k","d","n","l","p","h"]

--    it "solves part two of the puzzle" $ do
--      let moves = case (movesFromString file)
--                  of Right ms -> ms
--                     _ -> []
--      let line = ["a", "b", "c", "d", "e", "f", "g", "h"
--                 ,"i","j", "k", "l", "m", "n", "o", "p"]
--      let expected = ["p","n","h","a","j","o","e","k"
--                     ,"i","g","c","b","f","l","m","d"]
--      let danceMoves = flip dance moves
--      repeatTillSame danceMoves line `shouldBe` 60
--
--      iterate danceMoves line !! 40 `shouldBe` expected
