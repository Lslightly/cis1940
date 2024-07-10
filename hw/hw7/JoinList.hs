{-# LANGUAGE FlexibleInstances #-}
module JoinList where
import Sized (Sized (size), Size (Size), getSize)
-- data JoinListBasic a =  Empty
--                     |   Single a
--                     |   Append (JoinListBasic a) (JoinListBasic a)

-- jlbToList :: JoinListBasic a -> [a]
-- jlbToList Empty = []
-- jlbToList (Single a) = [a]
-- jlbToList (Append l1 l2) = jlbToList l1 ++ jlbToList l2

import Scrabble

import Data.Monoid

import Buffer
import Editor (runEditor, editor)

data JoinList m a = Empty
                |   Single m a
                |   Append m (JoinList m a) (JoinList m a)
        deriving (Eq, Show)

tl :: JoinList Size Char
tl = Append (Size 4)
    (Append (Size 3)
    (Single (Size 1) 'y')
    (Append (Size 2)
    (Single (Size 1) 'e')
    (Single (Size 1) 'a')))
    (Single (Size 1) 'h')

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m a) = m
tag (Append m _ _) = m
(+++) a b = Append (tag a <> tag b) a b

int :: (Sized m) => m -> Int
int = getSize . size
sizeofJL :: (Sized m, Monoid m) => JoinList m a -> Int
sizeofJL = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _
    | i < 0 = Nothing
indexJ i (Single _ a)
    | i > 0 = Nothing
    | i == 0 = Just a
indexJ i (Append m left right)
    | i >= int m = Nothing
    | i < leftsize = indexJ i left
    | otherwise = indexJ (i - leftsize) right
    where leftsize = sizeofJL left

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

testIndexJ :: Int -> Bool
testIndexJ i = indexJ i tl == (jlToList tl !!? i)

-- | testIndexJ
-- >>> testIndexJ (-1)
-- True
-- >>> testIndexJ 0
-- True
-- >>> testIndexJ 1
-- True
-- >>> testIndexJ 2
-- True
-- >>> testIndexJ 3
-- True
-- >>> testIndexJ 4
-- True
-- >>> testIndexJ 5
-- True

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 jl = jl
dropJ n jl | n >= sizeofJL jl = Empty
           | n < 0 = jl
dropJ n (Append m left right)
    | n >= leftsize = Empty +++ (dropJ (n - leftsize) right)
    | otherwise = (dropJ n left) +++ right
    where leftsize = sizeofJL left
testDropJ :: Int -> Bool
testDropJ n = jlToList (dropJ n tl) == drop n (jlToList tl)
-- | testDropJ
-- >>> testDropJ 1
-- True

-- >>> testDropJ 2
-- True

-- >>> testDropJ 3
-- True

-- >>> testDropJ 4
-- True

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n jl  | n >= sizeofJL jl = jl
            | n <= 0 = Empty
takeJ n (Append m left right)
    | n >= leftsize = left +++ newRight
    | otherwise = newLeft
    where leftsize = sizeofJL left
          newRight = takeJ (n - leftsize) right
          newLeft = takeJ n left
testTakeJ :: Int -> Bool
testTakeJ n = jlToList (takeJ n tl) == take n (jlToList tl)
-- | testTakeJ
-- >>> testTakeJ 0
-- True

-- >>> testTakeJ 1
-- True

-- >>> testTakeJ 2
-- True

-- >>> testTakeJ 3
-- True

-- >>> testTakeJ 4
-- True

-- >>> testTakeJ 5
-- True

-- >>> takeJ 1 tl
-- Append (Size 1) (Single (Size 1) 'y') Empty

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

createScoreSizeLine :: String -> JoinList (Score, Size) String
createScoreSizeLine s = Single (scoreString s, Size 1) s

testScoreLine :: JoinList Score String = Append (Score 23) 
    (Single (Score 9) "yay ")
    (Single (Score 14) "haskell!")
-- >>> scoreLine "yay " +++ scoreLine "haskell!" == testScoreLine
-- True

instance Buffer (JoinList (Score, Size) String) where
    toString = unlines . jlToList

    fromString = foldr ((+++) . createScoreSizeLine) Empty . lines

    line = indexJ

    replaceLine n l jl
        | 0 <= n && n < numLines jl = takeJ n jl +++ fromString l +++ dropJ (n+1) jl
        | otherwise = jl
    
    numLines = sizeofJL

    value = getScore . fst . tag

main = runEditor editor (fromString "yay\nhaskell!\n" :: (JoinList (Score, Size) String))

