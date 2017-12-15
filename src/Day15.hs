module Day15 where

import Numeric
import Data.Char

generator :: Int -> Int -> [Int]
generator factor input = nextValue : (generator factor nextValue)
  where nextValue = (input * factor) `mod` 2147483647

intToBinary :: Int -> String
intToBinary int = paddedBinary
  where paddedBinary = replicate (32 - length binary) '0' ++ binary
        binary = showIntAtBase 2 intToDigit int ""

compareBits :: String -> String -> Bool
compareBits a b = (drop 16 a) == (drop 16 b)

judge :: [(String, String)] -> [(String, String)]
judge = filter (\(a,b) -> compareBits a b)
