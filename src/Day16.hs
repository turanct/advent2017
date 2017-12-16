module Day16 where

import Data.List
import Text.ParserCombinators.Parsec hiding (Line)

type Amount = Int
type Position = Int
type Name = String
type Line = [Name]

data Move = Spin Amount | Exchange Position Position | Partner Name Name deriving (Eq, Show)

dance :: Line -> [Move] -> Line
dance = foldl danceMove

danceMove :: Line -> Move -> Line
danceMove l (Spin x) = spin x l
danceMove l (Exchange a b) = exchange a b l
danceMove l (Partner a b) = partner a b l

spin :: Amount -> Line -> Line
spin 0 l = l
spin x l = reverse $ take (length l) $ drop x $ cycle $ reverse l

exchange :: Position -> Position -> Line -> Line
exchange a b l
  | a == b = l
  | otherwise = begin ++ [elemB] ++ middle ++ [elemA] ++ end
  where aa = if a < b then a else b
        bb = if a < b then b else a
        elemA = l !! aa
        elemB = l !! bb
        begin = take aa l
        middle = take (bb - aa - 1) $ drop (aa + 1) l
        end = drop (bb + 1) l

partner :: Name -> Name -> Line -> Line
partner a b l = exchange pos1 pos2 l
  where pos1 = case elemIndex a l of Nothing -> 0
                                     Just x -> x
        pos2 = case elemIndex b l of Nothing -> 0
                                     Just x -> x

repeatTillSame :: Eq a => (a -> a) -> a -> Int
repeatTillSame f x = repeatTillSame' f x (0, [])
  where repeatTillSame' f x accum
          | result `elem` snd accum = fst accum
          | otherwise = repeatTillSame' f result ((fst accum + 1), result:(snd accum))
          where result = f x

movesFromString :: String -> Either ParseError [Move]
movesFromString = parse movesParser ""
  where movesParser = moveParser `sepBy` char ','
        moveParser = try spinParser <|> exchangeParser <|> partnerParser
        spinParser = do
          char 's'
          amount <- many1 digit
          return $ Spin (read amount)
        exchangeParser = do
          char 'x'
          a <- many1 digit
          char '/'
          b <- many1 digit
          return $ Exchange (read a) (read b)
        partnerParser = do
          char 'p'
          a <- many1 letter
          char '/'
          b <- many1 letter
          return $ Partner a b
