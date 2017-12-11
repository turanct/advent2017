module Day8 where

import Data.List
import qualified Data.Map.Strict as Map

type Register = String

data Modification = Increase Int | Decrease Int deriving (Eq, Show)

data Comparison = Equals
                | NotEquals
                | MoreThan
                | LessThan
                | MoreThanOrEquals
                | LessThanOrEquals
                deriving (Eq, Show)

data Condition = Condition Register Comparison Int deriving (Eq, Show)

data Instruction = Instruction
  { register :: Register
  , modification :: Modification
  , condition :: Condition
  } deriving (Eq, Show)

type Memory = Map.Map Register Int

readInstruction :: String -> Instruction
readInstruction a = Instruction r m c
  where parts = words a
        r = head parts
        m = case parts !! 1 of "inc" -> Increase amount
                               _ -> Decrease amount
        amount = read $ parts !! 2
        c = Condition reg comp int
        reg = parts !! 4
        comp = case parts !! 5 of "==" -> Equals
                                  "!=" -> NotEquals
                                  ">=" -> MoreThanOrEquals
                                  "<=" -> LessThanOrEquals
                                  ">" -> MoreThan
                                  _ -> LessThan
        int = read $ parts !! 6

initialState :: [Instruction] -> Memory
initialState is = Map.fromList [(r, 0) | r <- allregisters]
  where allregisters = sort $ nub $ ireg ++ creg
        ireg = map register is
        creg = map ((\(Condition cr _ _) -> cr) . condition) is

conditionIsMet :: Memory -> Condition -> Bool
conditionIsMet m (Condition r c i) = case Map.lookup r m
                                     of Nothing -> False
                                        Just registervalue -> comparison registervalue c i

comparison :: Int -> Comparison -> Int -> Bool
comparison r c i
  | c == Equals = r == i
  | c == NotEquals = r /= i
  | c == MoreThanOrEquals = r >= i
  | c == LessThanOrEquals = r <= i
  | c == MoreThan = r > i
  | c == LessThan = r < i

modifyRegister :: Memory -> Register -> Modification -> Memory
modifyRegister mem reg mod = case Map.lookup reg mem
                             of Nothing -> mem
                                Just registervalue -> Map.insert reg (runModification mod registervalue) mem

runModification :: Modification -> Int -> Int
runModification (Increase a) i = i + a
runModification (Decrease a) i = i - a

runInstruction :: Memory -> Instruction -> Memory
runInstruction m i = case conditionIsMet m (condition i)
                     of False -> m
                        True -> modifyRegister m (register i) (modification i)

runInstructions1 :: [Instruction] -> Memory
runInstructions1 is = foldl runInstruction (initialState is) is

solution1 :: [Instruction] -> Int
solution1 is = Map.foldl max 0 (runInstructions1 is)

runInstructions2 :: [Instruction] -> (Int, Memory)
runInstructions2 is = foldl runHighestValue (0, initialState is) is
  where runHighestValue s instruction = (highest, newmemory)
          where highest = max (fst s) (Map.foldl max 0 newmemory)
                newmemory = runInstruction (snd s) instruction

solution2 :: [Instruction] -> Int
solution2 is = fst $ runInstructions2 is
