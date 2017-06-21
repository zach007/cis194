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
