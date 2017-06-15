module Week1 where

toDigits :: Integer -> [Integer]
toDigits n = let (a,b) = divMod n 10
             in if a /= 0 then toDigits a ++ [b]
                      else [b]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = let (a,b) = divMod n 10
                in if a /=0 then b : toDigitsRev a
                           else b : []
