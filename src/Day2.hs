module Day2 where

type Cell = Int
type Row = [Cell]
type Spreadsheet = [Row]

checksum1 :: Spreadsheet -> Int
checksum1 = checksum rowdifference

checksum2 :: Spreadsheet -> Int
checksum2 = checksum rowdivision

checksum :: (Row -> Int) -> Spreadsheet -> Int
checksum rowcheck = foldr ((+) . rowcheck) 0

rowdifference :: Row -> Int
rowdifference cells = largest - smallest
  where largest = maximum cells
        smallest = minimum cells

rowdivision :: Row -> Int
rowdivision cells = head possibleQuotients
  where possibleQuotients = [ divident `div` divisor
                            | divident <- cells
                            , divisor <- cells
                            , divident `div` divisor /= 1
                            , divident `mod` divisor == 0 ]
