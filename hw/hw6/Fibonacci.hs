{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = map fst (iterate (\(x, y) -> (y, x+y)) (0, 1))

data Stream t = Cons t (Stream t)
streamToList :: Stream a -> [a]
streamToList (Cons t s) = t : streamToList s
instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat t = Cons t (streamRepeat t)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons t s) = Cons (f t) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed (streamFromSeed f (f seed))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = streamMap (\x -> last (takeWhile (\p ->  x `mod` (2 ^ p) == 0) [0..])) (streamFromSeed (+1) 1)

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))
instance Num (Stream Integer) where
    fromInteger n = Cons n (streamRepeat 0)
    negate = streamMap negate
    (+) (Cons a sa) (Cons b sb) = Cons (a+b) (sa + sb)
    (*) (Cons a0 a') b@(Cons b0 b') = Cons (a0 * b0) (streamMap (*a0) b' + a' * b)

instance Fractional (Stream Integer) where
    (/) a@(Cons a0 a') b@(Cons b0 b') = Cons (a0 `div` b0) (streamMap (`div` b0) a' - a / b * b')

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

{-
a b
c d
a c b d
-}
data Matrix = Matrix Integer Integer Integer Integer

instance Num Matrix where
    {-
    a b   a' b'
    c d   c' d'
    -}
    (*) (Matrix a c b d) (Matrix a' c' b' d') = Matrix (a*a'+b*c') (c*a'+d*c') (a*b'+b*d') (c*b'+d*d')
fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = case Matrix 1 1 1 0 ^ n of
    Matrix _ _ res _ -> res