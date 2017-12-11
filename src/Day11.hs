module Day11 where

import Data.List

data Direction = North | South | NorthEast | NorthWest | SouthEast | SouthWest deriving (Eq, Ord, Show)

data Path = Path { north :: Int
                 , south :: Int
                 , northeast :: Int
                 , northwest :: Int
                 , southeast :: Int
                 , southwest :: Int } deriving (Eq, Show)

directionsFromString :: String -> [Direction]
directionsFromString "" = []
directionsFromString s = dir : rec
  where dir = case takeWhile (/= ',') s
              of "n" -> North
                 "s" -> South
                 "ne" -> NorthEast
                 "nw" -> NorthWest
                 "se" -> SouthEast
                 _ -> SouthWest
        rec = directionsFromString $ drop 1 $ dropWhile (/= ',') s

opposite :: Direction -> Direction
opposite North = South
opposite South = North
opposite NorthEast = SouthWest
opposite NorthWest = SouthEast
opposite SouthEast = NorthWest
opposite SouthWest = NorthEast

filterOpposites :: [Direction] -> [Direction]
filterOpposites [] = []
filterOpposites (x:xs) = if opposite x `elem` xs
                         then filterOpposites $ delete (opposite x) xs
                         else x : filterOpposites xs

triangleLeft :: Direction -> Direction
triangleLeft North = SouthWest
triangleLeft South = NorthEast
triangleLeft NorthEast = NorthWest
triangleLeft NorthWest = South
triangleLeft SouthEast = North
triangleLeft SouthWest = SouthEast

triangleRight :: Direction -> Direction
triangleRight North = SouthEast
triangleRight South = NorthWest
triangleRight NorthEast = South
triangleRight NorthWest = NorthEast
triangleRight SouthEast = SouthWest
triangleRight SouthWest = North

simplifyLeftTriangles :: [Direction] -> [Direction]
simplifyLeftTriangles [] = []
simplifyLeftTriangles (x:xs) = if triangleLeft x `elem` xs
                               then simplifyLeftTriangles $ opposite (triangleRight x) : delete (triangleLeft x) xs
                               else x : simplifyLeftTriangles xs

simplifyRightTriangles :: [Direction] -> [Direction]
simplifyRightTriangles [] = []
simplifyRightTriangles (x:xs) = if triangleRight x `elem` xs
                               then simplifyRightTriangles $ opposite (triangleLeft x) : delete (triangleRight x) xs
                               else x : simplifyRightTriangles xs

filterDoubleSteps :: [Direction] -> [Direction]
filterDoubleSteps = filterOpposites . simplifyLeftTriangles . simplifyRightTriangles . simplifyLeftTriangles

part1 :: String -> Int
part1 = length . filterDoubleSteps . directionsFromString
