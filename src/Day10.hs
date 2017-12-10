module Day10 where

import Data.Char
import Data.Bits
import Numeric

type Circle = [Int]
type Position = Int
type Length = Int
type SkipSize = Int

data State = State { circle :: Circle
                   , position :: Position
                   , skipSize :: SkipSize
                   } deriving (Eq, Show)

knot :: State -> Length -> State
knot s l = State { circle = newcircle, position = newposition, skipSize = newskipsize}
  where newcircle = take clength $ drop remainingLength $ cycle $ reversed ++ rest
        clength = length (circle s)
        remainingLength = clength - (position s)
        reversed = reverse $ take l $ drop (position s) $ cycle (circle s)
        rest = take (clength - l) $ drop ((position s) + l) $ cycle (circle s)
        newposition = ((position s) + l + (skipSize s)) `mod` clength
        newskipsize = (skipSize s) + 1

checksum :: State -> Int
checksum s = foldl (*) 1 $ take 2 $ circle s

lengthsFromString :: String -> [Int]
lengthsFromString s = concat $ replicate 64 ascii
  where ascii = stringToAsciiList s ++ [17, 31, 73, 47, 23]
        stringToAsciiList = map (ord)

sparseHashToDenseHash :: [Int] -> [Int]
sparseHashToDenseHash xs = map (foldl xor 0) $ groupBy16 xs
  where groupBy16 [] = []
        groupBy16 xs
          | length xs < 16 = xs : []
          | otherwise = take 16 xs : (groupBy16 $ drop 16 xs)

asHex :: [Int] -> String
asHex is = concat $ map (pad . intToHex) is
  where intToHex i = showHex i ""
        pad [a] = ['0', a]
        pad s = s

knotHash :: String -> String
knotHash i = asHex $ sparseHashToDenseHash $ circle $ foldl knot state $ lengthsFromString i
  where state = State { circle = [0..255]
                      , position = 0
                      , skipSize = 0 }
