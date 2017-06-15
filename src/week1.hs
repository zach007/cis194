module Week1 where

toDigits :: Integer -> [Integer]
toDigits n
  | n > 0 = toDigits (div n 10) ++ [mod n 10]
  | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n > 0 = (mod n 10) : toDigitsRev (div n 10)
  | otherwise = []
