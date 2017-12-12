module Day12 where

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
