module Day5 where

type Position = Int
type Instruction = Int
type Instructions = [Instruction]
data State = Escaped | Jumping Position Instructions deriving (Eq, Show)

updateInstruction1 :: Instruction -> Instruction
updateInstruction1 = (+) 1

jump1 :: State -> State
jump1 = jump updateInstruction1

updateInstruction2 :: Instruction -> Instruction
updateInstruction2 i
  | i >= 3 = i - 1
  | otherwise = i + 1

jump2 :: State -> State
jump2 = jump updateInstruction2

jump :: (Instruction -> Instruction) -> State -> State
jump f Escaped = Escaped
jump f (Jumping p is) = nextState
  where nextState = if newPosition > length is
                    then Escaped
                    else Jumping newPosition newInstructions
        newPosition = p + instructionAtP
        instructionAtP = is !! (p - 1)
        newInstructions = take (p - 1) is ++ (f instructionAtP) : drop p is

countJumps :: (State -> State) -> State -> Int
countJumps jumpf state = countJumps' 0 state
  where countJumps' count Escaped = count
        countJumps' count state = countJumps' (count + 1) (jumpf state)
