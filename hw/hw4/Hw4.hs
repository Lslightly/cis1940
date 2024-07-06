module HW4 where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x-2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

-- | fun1'
-- >>> fun1 [1,3,4,5] == fun1' [1,3,4,5,7]
-- True

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n  | even n = n + fun2 (n `div` 2)
        | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=1) . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)
-- | fun2'
-- >>> fun2 10 == fun2' 10
-- True
-- >>> fun2 1 == fun2' 1
-- True
-- >>> fun2 2 == fun2' 2
-- True

-----------

data Tree a = Leaf
          |   Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree (a, Bool)
insert :: a -> Tree (a, Bool) -> Tree (a, Bool)
height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

isFull :: Tree (a, Bool) -> Bool
isFull Leaf = True
isFull (Node _ _ (_, full) _) = full

insert a Leaf = Node 0 Leaf (a, True) Leaf
insert a (Node h left (root, True) right) = Node (h+1) (insert a left) (root, False) right
insert a (Node h left (root, False) right)
  | height left == height right && not (isFull left) = Node h (insert a left) (root, False) right
  | height left == height right && isFull left = Node h left (root, isFull (insert a right)) (insert a right)
  | height left > height right = Node h left (root, isFull (insert a right) && height left == height (insert a right)) (insert a right)
  | height left < height right = Node h (insert a left) (root, False) right

deleteFull :: Tree (a, Bool) -> Tree a
deleteFull Leaf = Leaf
deleteFull (Node h left (root, _) right) = Node h (deleteFull left) root (deleteFull right)

foldTree = foldr insert Leaf

----------

xor :: [Bool] -> Bool
xor = foldr (\x z -> if x then not z else z) False 
-- | xor
-- >>> xor [False, True, False]
-- True
-- >>> xor [False, True, False, False, True]
-- False

map' :: (a->b) -> [a] -> [b]
map' f = foldr (\x s -> f x : s) []
-- | map'
-- >>> map' (\x -> x+1) [1, 2, 3] == map (\x -> x+1) [1, 2, 3]
-- True