module Week1 where

toDigit :: Integer -> [Integer]
toDigit n
  | n > 0 = toDigit (div n 10) ++ [mod n 10]
  | otherwise = []

toDigitRev :: Integer -> [Integer]
toDigitRev n
  | n > 0 = (mod n 10) : toDigitRev (div n 10)
  | otherwise = []

