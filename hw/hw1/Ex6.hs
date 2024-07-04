module Ex6 where
import Ex5 (hanoi)
-- | hanoi four pegs
-- >>> length (hanoi4 2 "a" "b" "c" "d")
-- 3
-- >>> length (hanoi4 3 "a" "b" "c" "d")
-- 5
-- >>> length (hanoi4 15 "a" "b" "c" "d")
-- 129
type Peg = String
type Move = (Peg, Peg)
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
k = 3 -- I don't know why
hanoi4 n src tgt tmp1 tmp2
  | n == 1 = [(src, tgt)]
  | n == 2 = [(src, tmp1), (src, tgt), (tmp1, tgt)]
  | otherwise = hanoi4 (n-k) src tmp1 tgt tmp2 ++ hanoi k src tgt tmp2 ++ hanoi4 (n-k) tmp1 tgt src tmp2