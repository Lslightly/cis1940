{-# LANGUAGE TypeSynonymInstances #-}
module Calc where
import ExprT
import Parser (parseExp)
import Data.Maybe (isNothing)
import qualified StackVM

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Mul e1 e2) = eval e1 * eval e2
eval (Add e1 e2) = eval e1 + eval e2
-- | eval
-- >>> eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
-- 20

evalStr :: String -> Maybe Integer
evalStr = (
    \x -> case x of
    Nothing -> Nothing
    Just (x) -> Just (eval x)) . parseExp Lit Add Mul 
-- | evalStr
-- >>> evalStr "(2+3)*4"
-- Just 20
-- >>> evalStr "2+3*"
-- Nothing

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul
-- | Expr
-- >>> (mul (add (lit 2) (lit 3)) (lit 4) :: ExprT) == Mul (Add (Lit 2) (Lit 3)) (Lit 4)
-- True

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (>0)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax a) (MinMax b) = MinMax (max a b)
    mul (MinMax a) (MinMax b) = MinMax (min a b)

instance Expr Mod7 where
    lit = Mod7
    add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
    mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
    
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

-- | test
-- >>> testInteger
-- Just (-7)
-- >>> testBool
-- Just True
-- >>> testMM
-- Just (MinMax 5)
-- >>> testSat
-- Just (Mod7 0)

instance Expr StackVM.Program where
    lit n = [StackVM.PushI n]
    add p1 p2 = p1 ++ p2 ++ [StackVM.Add]
    mul p1 p2 = p1 ++ p2 ++ [StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile s = parseExp lit add mul s :: Maybe StackVM.Program

exec :: String -> Either String StackVM.StackVal
exec s = case compile s of
    Just p -> StackVM.stackVM p
    Nothing -> Left "compile error"

-- | exec
-- >>> exec "(3 * -4) + 5"
-- Right (IVal (-7))
