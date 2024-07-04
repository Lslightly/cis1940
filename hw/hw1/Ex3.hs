{-# OPTIONS_GHC -Wall #-}
module Ex3 where
import Ex1 (toDigits)
-- | sumDigits
-- >>> sumDigits [16,7,12,5]
-- 22
sumDigits :: [Integer] -> Integer
sumNum :: Integer -> Integer
sumNum n = sum (toDigits n)
sumDigits l = sum (map sumNum l)