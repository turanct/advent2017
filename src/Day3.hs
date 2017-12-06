module Day3 where

type Square = Int
type Spiral = Int
type Coordinate = (Int, Int)

-- number of squares on each side of the spiral
sideSquares :: Spiral -> Int
sideSquares sp = (sp * 2) - 1

-- the coordinates for all squares in the given spiral
coordinatesForSpiral :: Spiral -> [Coordinate]
coordinatesForSpiral 1 = [(0, 0)]
coordinatesForSpiral sp = right ++ top ++ left ++ bottom
  where fixed = sp - 1
        sides = sideSquares sp - 1
        right = [(x, y) | x <- [fixed], y <- take sides [-1*(fixed-1)..]]
        top = [(x, y) | x <- reverse (take sides [-1*fixed..]), y <- [fixed]]
        left = [(x, y) | x <- [-1*fixed], y <- reverse(take sides [-1*fixed..])]
        bottom = [(x, y) | x <- take sides [-1*fixed+1..], y <- [-1*fixed]]

-- an endless list of coordinates (the complete spiral)
endlessSpiral :: [Coordinate]
endlessSpiral = endlessSpiral' 1
endlessSpiral' start = coordinatesForSpiral start ++ endlessSpiral' (start + 1)

-- the spiral as listed in the example on Advent Of Code, counting from 1
spiralCountingFrom1 :: [(Square, Coordinate)]
spiralCountingFrom1 = zip [1..] endlessSpiral

-- solution to problem 1
steps1 :: Square -> Int
steps1 sq = abs (fst coordinate) + abs (snd coordinate)
  where coordinate = snd $ head $ drop (sq - 1) spiralCountingFrom1

-- for problem 2 we need to calculate neighbours for a coordinate
neighbours :: Coordinate -> [Coordinate]
neighbours (x, y) = [ (x + 1, y)
                    , (x + 1, y + 1)
                    , (x, y + 1)
                    , (x - 1, y + 1)
                    , (x - 1, y)
                    , (x - 1, y - 1)
                    , (x, y - 1)
                    , (x + 1, y - 1) ]

-- this is the function we'll use to foldl over a list of coordinates
-- to find the sums of the surrounding neighbours that were calculated.
-- it remembers the coordinates it already visited, and the sums that were
-- calculated for them by passing on 'state' in the recursive calls.
sumNeighbours :: [(Coordinate, Int)] -> Coordinate -> [(Coordinate, Int)]
sumNeighbours state coord = (coord, summer neighbourvalues):state
  where neighbourvalues = map findvalue $ neighbours coord
        summer = foldr (+) 0
        findvalue coord = foldr (+) 0 $ map snd $ filtercoord coord
        filtercoord coord = filter ((== coord) . fst) state

-- we take an endless list of spiral coordinates, take the first 60 of them
-- and start folding over that list with the above fold function. We then find
-- we output the first number we found that was heigher than our puzzle input.
neighbourSpiral = head $ filter (> 265149) neighboursumspiral
  where first60coords = take 60 $ drop 1 endlessSpiral
        neighboursumspiral = map snd $ reverse $ foldl sumNeighbours [((0, 0), 1)] first60coords
