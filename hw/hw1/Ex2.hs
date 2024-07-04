{-# OPTIONS_GHC -Wall #-}
module Ex2 where
-- | doubleEveryOther
-- >>> doubleEveryOther [8,7,6,5]
-- [16,7,12,5]
-- >>> doubleEveryOther [1,2,3]
-- [1,4,3]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev [x] = [x]
doubleEveryOtherRev (x:y:zs) = x : y * 2 : doubleEveryOtherRev zs
doubleEveryOther xs = reverse (doubleEveryOtherRev (reverse xs))
