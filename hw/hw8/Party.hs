{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where
import Data.Tree
import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (f + empFun e)

instance Semigroup GuestList where
    (<>) (GL l1 f1) (GL l2 f2) = GL (l1 ++ l2) (f1 + f2)

instance Monoid GuestList where
    mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: b -> (a -> [b] -> b) -> Tree a -> b
treeFold e f (Node a cs)
    | null cs = f a []
    | otherwise = f a (map (treeFold e f) cs)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e gls = (maxNoBosses, foldr moreFun maxNoBosses withBosses)
    where   noBosses = map snd gls
            withBosses = map fst gls
            maxNoBosses = foldr moreFun (GL [e] (empFun e)) noBosses

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold (GL [] 0, GL [] 0) nextLevel

-- | maxFun
-- >>> maxFun testCompany2
-- GL [Emp {empName = "Sarah", empFun = 17}] 17

main = readFile "company.txt" >>= (\s -> print (maxFun (read s :: Tree Employee)))
