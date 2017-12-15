module Day14 where

import Day10
import Numeric

gridKnotHash :: String -> Int -> [String]
gridKnotHash s i = map knotHash $ zipWith concatStrings (repeat s) [0..i-1]
  where concatStrings x y = x ++ "-" ++ (show y)

hexStringToBinString :: String -> String
hexStringToBinString s = concat $ map hexToBin s

hexToBin :: Char -> String
hexToBin '0' = "0000"
hexToBin '1' = "0001"
hexToBin '2' = "0010"
hexToBin '3' = "0011"
hexToBin '4' = "0100"
hexToBin '5' = "0101"
hexToBin '6' = "0110"
hexToBin '7' = "0111"
hexToBin '8' = "1000"
hexToBin '9' = "1001"
hexToBin 'a' = "1010"
hexToBin 'b' = "1011"
hexToBin 'c' = "1100"
hexToBin 'd' = "1101"
hexToBin 'e' = "1110"
hexToBin 'f' = "1111"

onesInGrid :: [String] -> Int
onesInGrid ss = foldl (+) 0 $ map length $ map (filter (== '1')) ss

type Line = Int
type CharNum = Int
type Coord = (Line, CharNum)
type Region = [Coord]

regionsInGrid :: [String] -> [Region]
regionsInGrid ss = keepMergingRegions 0 $ groupByRegion $ flattenlines $ gridLines ss
  where keepMergingRegions x regions
          | length merged == x = merged
          | otherwise = keepMergingRegions (length merged) merged
          where merged = mergeRegions regions

gridLines :: [String] -> [[Coord]]
gridLines ss = mapLineNrs 1 $ map (filterZeros . (mapPosition 1)) ss
  where mapPosition x [] = []
        mapPosition x (s:ss) = (x, s) : mapPosition (x+1) ss
        filterZeros [] = []
        filterZeros ((x, y):xs) = if y == '0' then filterZeros xs else (x) : filterZeros xs
        mapLineNrs x [] = []
        mapLineNrs x (l:ls) = (map (\(y) -> (x,y)) l) : mapLineNrs (x+1) ls

flattenlines :: [[Coord]] -> [Coord]
flattenlines ls = foldl (++) [] ls

groupByRegion :: [Coord] -> [Region]
groupByRegion = groupByRegion' []

groupByRegion' :: [Region] -> [Coord] -> [Region]
groupByRegion' gs [] = gs
groupByRegion' gs (c:cs) = groupByRegion' newregions cs
  where newregions = c `addToCorrectRegion` gs
        addToCorrectRegion c [] = [[c]]
        addToCorrectRegion c (g:gs) = if c `hasNeighbourInRegion` g
                                     then (c:g) : gs
                                     else g : addToCorrectRegion c gs

hasNeighbourInRegion :: Coord -> Region -> Bool
hasNeighbourInRegion c gcs = any (neighbour c) gcs
neighbour a b
  | fst a == fst b && abs (snd a - snd b) == 1 = True
  | abs (fst a - fst b) == 1 && snd a == snd b = True
  | otherwise = False

sameRegion :: Region -> Region -> Bool
sameRegion [] _ = False
sameRegion (a:as) b = a `hasNeighbourInRegion` b || sameRegion as b

mergeRegions :: [Region] -> [Region]
mergeRegions [] = []
mergeRegions (g:gs) = merged : mergeRegions (filter (not . sameRegion g) gs)
  where merged = g ++ concat (filter (sameRegion g) gs)
