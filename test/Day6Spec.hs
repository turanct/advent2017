module Day6Spec where

import Test.Hspec
import Day6

spec :: Spec
spec = do
  describe "findHighestBank" $ do
    it "finds the highest bank in very small memory" $ do
      let memory = [1]
      findHighestBank memory `shouldBe` (1, 1, [0])

    it "finds the highest bank in small memory" $ do
      let memory = [1, 2, 4, 2, 1]
      findHighestBank memory `shouldBe` (3, 4, [1, 2, 0, 2, 1])

    it "takes the first bank, if two banks are highest" $ do
      let memory = [1, 2, 4, 10, 9, 10, 2]
      findHighestBank memory `shouldBe` (4, 10, [1, 2, 4, 0, 9, 10, 2])

  describe "distribute" $ do
    it "distributes a small value over registers" $ do
      distribute 3 4 [1, 2, 0, 2, 1] `shouldBe` [2, 3, 0, 3, 2]

  describe "reallocate" $ do
    it "reallocates memory" $ do
      reallocate [0, 2, 7, 0] `shouldBe` [2, 4, 1, 2]

  describe "stepsUntilSameState" $ do
    it "finds the number of steps for example input" $ do
      stepsUntilSameState [0, 2, 7, 0] `shouldBe` 5

--    file <- runIO $ readFile "test/day6.txt"
--
--    it "solves the puzzle" $ do
--      let memory = map read $ words $ head $ lines file
--
--      stepsUntilSameState memory `shouldBe` 12841
--
--    it "solves the second puzzle" $ do
--      let memory = map read $ words $ head $ lines file
--
--      stepsUntilCycle memory `shouldBe` 8038
