module Week4 where
import  Data.List
fun1' :: [Integer] -> Integer
--fun1' xs = foldr (*) 1 $ map((-)2) $ filter (even) xs
fun1' = product . map (subtract 2) .filter even

fun2' :: Integer -> Integer
fun2' = sum
        .filter (even)
        .takeWhile(/= 1)
        .iterate(\n-> if even n then n `div` 2 else 3 * n + 1)

data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
--insert into Leaf
--insert into Node -> left --> if the right is not empty null
--insert into Node -> right --> if the left is not empty
--insert into Node -> default left
--after insert ,height + 1  and then still a balance tree
  where
    insert :: a -> Tree a -> Tree a
    insert x Leaf = Node 0 Leaf x Leaf
    insert x (Node height left val right)
      | treeHeight left >= treeHeight right =
        let new_right = insert x right
        in Node (treeHeight new_right + 1) left val (insert x right)
      | otherwise =
        let new_left = insert x left
        in Node (treeHeight new_left + 1) (insert x left) val right

treeHeight :: Tree a -> Integer
treeHeight Leaf = -1
treeHeight (Node height _ _ _)= height

xor :: [Bool] -> Bool
xor = foldr eachOne False
  where
    eachOne :: Bool -> Bool -> Bool
    eachOne True False = True
    eachOne True True = False
    eachOne False True = True
    eachOne False False = False

xor2 :: Bool -> Bool -> Bool
xor2 a b = (a || b) && not (a && b)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []


sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) . (*2)) $ [1..n] \\ sieve
  where sieve = map (\(i, j) -> i + j + 2*i*j)
                . filter (\(i, j) -> i + j + 2*i*j <= n)
                $ cartProd [1..n] [1..n]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
