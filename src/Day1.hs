module Day1 where

calculateCode1 :: [Int] -> Int
calculateCode1 digits = calculateCode digits rotatedDigits
  where rotatedDigits = drop 1 digits ++ take 1 digits

calculateCode2 :: [Int] -> Int
calculateCode2 digits = calculateCode digits rotatedDigits
  where firstHalf = take (length digits `div` 2) digits
        lastHalf = drop (length digits `div` 2) digits
        rotatedDigits = lastHalf ++ firstHalf

calculateCode :: [Int] -> [Int] -> Int
calculateCode xs ys = sum $ zipWith digitIfEqual xs ys

digitIfEqual :: Int -> Int -> Int
digitIfEqual x y
  | x == y = x
  | otherwise = 0
