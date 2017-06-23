module Week1 where

toDigit :: Integer -> [Integer]
toDigit n
  | n > 0 = toDigit (div n 10) ++ [mod n 10]
  | otherwise = []

toDigitRev :: Integer -> [Integer]
toDigitRev n
  | n > 0 = (mod n 10) : toDigitRev (div n 10)
  | otherwise = []

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs
  | odd $ length xs = doubleFromLeft xs
  | otherwise = reverse $ doubleFromLeft $ reverse xs

doubleFromLeft :: [Integer] -> [Integer]
doubleFromLeft [] = []
doubleFromLeft [x] = [x]
doubleFromLeft (x : y : z) = x : 2 * y : doubleFromLeft z

{--better implement of double a 2nd list --}
double2Nd :: [Integer] -> [Integer]
double2Nd = zipWith ($) (cycle [id,(*2)])

sumDigit :: [Integer] -> Integer
sumDigit xs = foldl1 (+) $ map (foldl1 (+)) (filter (/=[]) $ map (toDigit) xs)

validate :: Integer -> Bool
validate n = let result = mod (sumDigit $ doubleEveryOther $ toDigit n) 10
             in if result == 0 then True
                               else False
