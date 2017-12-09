module Day9 where

import Text.ParserCombinators.Parsec

data Sequence = Garbage String
              | Group [Sequence] deriving (Eq, Show)

garbageParser = do
  char '<'
  many escapedCharParser
  garbage <- many garbageContentParser
  char '>'
  return (Garbage garbage)

garbageContentParser = do
  char <- noneOf ">"
  many escapedCharParser
  return char

escapedCharParser = do
  char '!'
  escaped <- anyChar
  return escaped

groupParser = do
  char '{'
  subsequences <- subsequenceParser `sepBy` (char ',')
  char '}'
  return (Group subsequences)

subsequenceParser = try (garbageParser) <|> groupParser

parseString :: String -> Either ParseError Sequence
parseString = parse subsequenceParser ""

score :: Sequence -> Int
score = score' 1
  where score' s (Garbage _) = 0
        score' s (Group subgroups) = s + sum [ score' (s + 1) sg | sg <- subgroups ]

countGarbage :: Sequence -> Int
countGarbage (Garbage s) = length s
countGarbage (Group sg) = sum $ map countGarbage sg
