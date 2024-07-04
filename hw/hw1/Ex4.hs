{-# OPTIONS_GHC -Wall #-}
module Ex4 where
import Ex1 (toDigits)
import Ex2 (doubleEveryOther)
import Ex3 (sumDigits)
-- | validate
-- >>> validate 4012888888881881
-- True
-- >>> validate 4012888888881882 
-- False

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0