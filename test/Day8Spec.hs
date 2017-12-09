module Day8Spec where

import Test.Hspec
import Day8
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
  describe "readInstruction" $ do
    it "reads a simple instruction 1" $ do
      let string = "b inc 5 if a > 1"
      let instruction = Instruction "b" (Increase 5) (Condition "a" MoreThan 1)
      readInstruction string `shouldBe` instruction

    it "reads a simple instruction 2" $ do
      let string = "c dec -10 if a >= 1"
      let instruction = Instruction "c" (Decrease (-10)) (Condition "a" MoreThanOrEquals 1)
      readInstruction string `shouldBe` instruction

  describe "initial state" $ do
    it "creates a hashmap of initial register values 1" $ do
      let instructions = []
      initialState instructions `shouldBe` Map.fromList []

    it "creates a hashmap of initial register values 2" $ do
      let instructions = [ Instruction "b" (Increase 5) (Condition "a" MoreThan 1)
                         , Instruction "a" (Increase 1) (Condition "b" LessThan 5)
                         , Instruction "c" (Decrease (-10)) (Condition "a" MoreThanOrEquals 1)
                         , Instruction "c" (Increase (-20)) (Condition "c" Equals 10) ]
      initialState instructions `shouldBe` (Map.fromList [("a", 0), ("b", 0), ("c", 0)])

  describe "run instruction" $ do
    it "does no updates to memory if condition not met" $ do
      let memory = Map.fromList [("a", 0), ("b", 1), ("c", 2)]
      let instruction = Instruction "b" (Increase 1) (Condition "c" LessThan 1)
      runInstruction memory instruction `shouldBe` memory

    it "updates memory according to instruction" $ do
      let memory = Map.fromList [("a", 0), ("b", 1), ("c", 2)]
      let instruction = Instruction "b" (Increase 1) (Condition "c" LessThan 5)
      runInstruction memory instruction `shouldBe` Map.fromList [("a", 0), ("b", 2), ("c", 2)]

  describe "run all instructions" $ do
    it "runs instructions front to back" $ do
      let instructions = [ Instruction "b" (Increase 5) (Condition "a" MoreThan 1)
                         , Instruction "a" (Increase 1) (Condition "b" LessThan 5)
                         , Instruction "c" (Decrease (-10)) (Condition "a" MoreThanOrEquals 1)
                         , Instruction "c" (Increase (-20)) (Condition "c" Equals 10) ]
      runInstructions1 instructions `shouldBe` Map.fromList [("a",1),("b",0),("c",-10)]

  describe "solution 1" $ do
    it "finds the highest value in any register" $ do
      let instructions = [ Instruction "b" (Increase 5) (Condition "a" MoreThan 1)
                         , Instruction "a" (Increase 1) (Condition "b" LessThan 5)
                         , Instruction "c" (Decrease (-10)) (Condition "a" MoreThanOrEquals 1)
                         , Instruction "c" (Increase (-20)) (Condition "c" Equals 10) ]
      solution1 instructions `shouldBe` 1

    file <- runIO $ readFile "test/day8.txt"
    it "solves the puzzle" $ do
      let instructions = map readInstruction $ lines file
      solution1 instructions `shouldBe` 6343

  describe "solution 2" $ do
    it "returns the highest value ever in memory" $ do
      let instructions = [ Instruction "b" (Increase 5) (Condition "a" MoreThan 1)
                         , Instruction "a" (Increase 1) (Condition "b" LessThan 5)
                         , Instruction "c" (Decrease (-10)) (Condition "a" MoreThanOrEquals 1)
                         , Instruction "c" (Increase (-20)) (Condition "c" Equals 10) ]
      solution2 instructions `shouldBe` 10

    file <- runIO $ readFile "test/day8.txt"
    it "solves the puzzle" $ do
      let instructions = map readInstruction $ lines file
      solution2 instructions `shouldBe` 7184
