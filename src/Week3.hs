module Week3 where


skips :: [a] -> [[a]]
skips lst = [each i lst | i <- [1..length lst]]

each :: Int -> [a] -> [a]
each n lst = [lst !! i | i <- [n-1, n-1+n..length lst - 1]]


localMaxima :: [Integer] -> [Integer]
localMaxima xs = foldl1 (++) $ map (maxima) [take 3 $ drop n xs | n <- [0..length xs]]


maxima :: [Integer] -> [Integer]
maxima xs
  | length xs /= 3 = []
  | otherwise = theMaxAmongThree xs

theMaxAmongThree :: [Integer] -> [Integer]
theMaxAmongThree xs
  | xs!!0 < xs!! 1 && xs!! 1 > xs !!2  = [xs!!1]
  | otherwise = []


