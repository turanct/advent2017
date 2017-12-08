module Day7 where

type Name = String
type Weight = Int
data Program = Program Name Weight [Name] deriving (Eq, Show)

readProgram :: String -> Program
readProgram a = Program name weight children
  where name = takeWhile (/= ' ') a
        weight = read $ takeWhile (/= ')') $ drop 1 $ dropWhile (/= '(') a
        children = splitByComma $ drop 2 $ dropWhile (/= '>') a

splitByComma :: String -> [String]
splitByComma "" = []
splitByComma s = firstpart : splitByComma rest
  where firstpart = takeWhile (/= ',') $ dropWhile (== ' ') s
        rest = drop 1 $ dropWhile (/= ',') s
