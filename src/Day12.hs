module Day12 where

import Data.List

type Process = String
type Connection = (Process, Process)

readConnections :: String -> [Connection]
readConnections a = zip (repeat currentConnection) connections
  where currentConnection = takeWhile (/= ' ') a
        connections = splitByComma $ drop 2 $ dropWhile (/= '>') a

splitByComma :: String -> [String]
splitByComma "" = []
splitByComma s = firstpart : splitByComma rest
  where firstpart = takeWhile (/= ',') $ dropWhile (== ' ') s
        rest = drop 1 $ dropWhile (/= ',') s

directConnections :: [Connection] -> Process -> [Process]
directConnections cs p = map snd $ filter ((== p) . fst) cs

findGroup :: [Connection] -> Process -> [Process]
findGroup cs p = findGroup' cs [] p
  where findGroup' cs ps p
          | p `elem` ps = ps
          | otherwise = nub $ concat $ map (findGroup' cs (p:ps)) (directConnections cs p)

part1 :: [Connection] -> Process -> Int
part1 cs p = length $ findGroup cs p

findGroups :: [Connection] -> [[Process]]
findGroups cs = foldl addGroupIfNotPresent [] cs
  where addGroupIfNotPresent groups connection
          | fst connection `elem` concat groups = groups
          | otherwise = findGroup cs (fst connection) : groups

part2 :: [Connection] -> Int
part2 = length . findGroups
