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

childOf :: Program -> Program -> Bool
childOf (Program name _ _) (Program _ _ children) = name `elem` children

topProgram :: [Program] -> Program
topProgram = head . topPrograms
  where topPrograms ps = [p | p <- ps, all (\x -> p `childOf` x == False) ps]

programByName :: [Program] -> Name -> Program
programByName ps n = head $ filter (\(Program name _ _) -> name == n) ps

weight :: [Program] -> Name -> Weight
weight ps p = programweight + childrenweight
  where programweight = (\(Program _ w _) -> w) currentprogram
        currentprogram = programByName ps p
        childrenweight = sum $ map (weight ps) children
        children = (\(Program _ _ c) -> c) currentprogram

balanced :: [Program] -> Name -> Bool
balanced ps p = balanced' $ programByName ps p
  where balanced' (Program _ _ []) = True
        balanced' (Program _ _ c) = allthesame $ map (weight ps) c
        allthesame xs = and $ map (== head xs) (tail xs)
