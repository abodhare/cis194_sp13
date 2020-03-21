import Data.List

fun1 :: [Integer] -> Integer
fun1 = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (/=1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = -1
height (Node x _ _ _) = x

treeInsert :: a -> Tree a -> Tree a
treeInsert x Leaf = Node 0 Leaf x Leaf
treeInsert x (Node h l y r)
    | height l < height r = Node h (treeInsert x l) y r
    | height l == height r = Node (if height (treeInsert x l) > height r then h + 1 else h) (treeInsert x l) y r
    | otherwise = Node h l y (treeInsert x r)

foldTree :: [a] -> Tree a
foldTree = foldr treeInsert Leaf

xor :: [Bool] -> Bool
xor = foldr (/=) False

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) . (*2)) ([1..n] \\ filter (<=n) [i + j + 2*i*j | i <- [1..n], j <- [1..n]])