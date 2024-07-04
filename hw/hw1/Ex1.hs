{-# OPTIONS_GHC -Wall #-}
module Ex1 where
-- | toDigits
-- >>> toDigits 1234
-- [1,2,3,4]
-- >>> toDigitsRev 1234
-- [4,3,2,1]
-- >>> toDigits 0
-- []
-- >>> toDigits (-17)
-- []
toDigits :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n < 0 || n == 0 = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)
toDigits n = reverse (toDigitsRev n)
