module Scrabble where

newtype Score = Score Int
    deriving (Eq, Ord, Num, Show)

instance Semigroup Score where
    (Score a) <> (Score b) = Score (a+b)

instance Monoid Score where
    mempty = Score 0

score :: Char -> Score
scoreString :: String -> Score
letterScore :: Char -> Int
letterScore c
    | c `elem` "aeilnorstu" = 1
    | c `elem` "dg" = 2
    | c `elem` "bcmp" = 3
    | c `elem` "fhvwy" = 4
    | c `elem` "k" = 5
    | c `elem` "jx" = 8
    | c `elem` "qz" = 10
    | otherwise = 0
score c = Score (letterScore c)
scoreString = mconcat . map score

getScore :: Score -> Int
getScore (Score i) = i


