module Day13 where

type Range = Int
type Depth = Int
type Firewall = [(Depth, Range)]

caughtOnLevel :: Depth -> Range -> Bool
caughtOnLevel _ 0 = False
caughtOnLevel d r = position `mod` steps == 1
  where position = d + 1
        steps = r * 2 - 2

firewallFromFile :: String -> [(Depth, Range)]
firewallFromFile file = map levelFromString $ lines file
  where levelFromString s = (d, r)
          where d = read $ takeWhile (/= ':') s
                r = read $ drop 2 $ dropWhile (/= ':') s

severity :: Firewall -> Int
severity firewall = foldl (\x y -> x + (fst y * snd y)) 0 caughtlevels
  where caughtlevels = caughtOnLevels firewall
        caughtOnLevels f = filter (\(d, r) -> caughtOnLevel d r) f

waitBeforeGoing :: Firewall -> Int
waitBeforeGoing firewall = head $ [ s | s <- [0..], all (notCaught s) firewall ]
  where notCaught s (d, r) = not $ caughtOnLevel (d+s) r
