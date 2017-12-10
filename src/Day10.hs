module Day10 where

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
