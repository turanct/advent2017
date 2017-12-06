module Day5Spec where

import Test.Hspec
import Day5

spec :: Spec
spec = do
  describe "jump1" $ do
    it "jumps and updates instructions 1" $ do
      let instructions = [0, 3, 0, 1, -3]
      let state = Jumping 1 instructions
      jump1 state `shouldBe` Jumping 1 [1, 3, 0, 1, -3]

    it "jumps and updates instructions 2" $ do
      let instructions = [1, 3, 0, 1, -3]
      let state = Jumping 1 instructions
      jump1 state `shouldBe` Jumping 2 [2, 3, 0, 1, -3]

    it "jumps and updates instructions 3" $ do
      let instructions = [2, 3, 0, 1, -3]
      let state = Jumping 2 instructions
      jump1 state `shouldBe` Jumping 5 [2, 4, 0, 1, -3]

    it "jumps and updates instructions 4" $ do
      let instructions = [2, 4, 0, 1, -3]
      let state = Jumping 5 instructions
      jump1 state `shouldBe` Jumping 2 [2, 4, 0, 1, -2]

    it "jumps and updates instructions 5" $ do
      let instructions = [2, 4, 0, 1, -2]
      let state = Jumping 2 instructions
      jump1 state `shouldBe` Escaped

--  describe "countJumps" $ do
--    it "counts jumps" $ do
--      let instructions = [0, 3, 0, 1, -3]
--      let state = Jumping 1 instructions
--
--      countJumps jump1 state `shouldBe` 5
--
--    file <- runIO $ readFile "test/day5.txt"
--
--    it "solves puzzle 1" $ do
--      let instructions = map read $ lines file
--      let initialState = Jumping 1 instructions
--
--      countJumps jump1 initialState `shouldBe` 374269
--
--    it "solves puzzle 2" $ do
--      let instructions = map read $ lines file
--      let initialState = Jumping 1 instructions
--
--      countJumps jump2 initialState `shouldBe` 27720699
