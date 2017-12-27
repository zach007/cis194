module Week6 where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib(n-2)


fibs1 :: [Integer]
fibs1 = map fib [0,1..]
{-
  take 5 element of fibsl

  fibsl = fib 4 + fib 3
        = fib 3 + fib 2  + fib 2 + fib 1
        = fib 2 + fib 1 + fib 1 + fib 0 + fib 1 + fib 0 + fib1
-}

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a )where
    show = show . take 20 .streamToList

streamToList :: Stream a -> [a]
streamToList (Cons y c) = y : streamToList c


streamRepeat :: a -> Stream a
streamRepeat n = Cons n (streamRepeat n)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x ys) = Cons (f x) (streamMap f ys)


streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f y = Cons y (streamFromSeed f (f y))

nats :: Stream Integer
nats =streamFromSeed (+1) 0



