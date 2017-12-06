module Day6 where

import Data.List

type Bank = Int
type Block = Int
type Memory = [Block]

findHighestBank :: Memory -> (Bank, Block, Memory)
findHighestBank memory = (bank, biggestBlock, newMemory)
  where biggestBlock = maximum memory
        bank = case elemIndex biggestBlock memory
               of Just x -> x + 1
                  _ -> 1
        newMemory = take (bank - 1) memory ++ [0] ++ drop bank memory

distribute :: Bank -> Block -> Memory -> Memory
distribute _ 0 me = me
distribute ba bl me = distribute bank (bl - 1) newmemory
  where newmemory = take (bank - 1) me ++ [bankvalue + 1] ++ drop bank me
        bankvalue = me !! (bank - 1)
        bank | ba == length me = 1
             | otherwise = ba + 1

reallocate :: Memory -> Memory
reallocate memory = distribute bank block mem
  where (bank, block, mem) = findHighestBank memory

stepsUntilSameState :: Memory -> Int
stepsUntilSameState m = stepsUntilSameState' m []
  where stepsUntilSameState' m ms
          | allocation `elem` ms = length ms + 1
          | otherwise = stepsUntilSameState' allocation (allocation:ms)
          where allocation = reallocate m

reallocationCycle :: Memory -> [Memory]
reallocationCycle m = newM : reallocationCycle newM
  where newM = reallocate m

stepsUntilCycle :: Memory -> Int
stepsUntilCycle m = (+) 1 $ length $ takeWhile (/= reference) $ drop firststeps cycle
  where firststeps = stepsUntilSameState m
        cycle = reallocationCycle m
        reference = head $ drop (firststeps - 1) cycle
