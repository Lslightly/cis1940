{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where


import           Data.Char
import          Control.Applicative

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)
instance Functor Parser where
    fmap g (Parser f) = Parser (fmap (first g) . f)

instance Applicative Parser where
    pure f = Parser (\s -> Just (f, s))
    p1 <*> p2 = Parser (\s -> (
        case runParser p1 s of
            Nothing -> Nothing
            Just (f, remain) ->
                case runParser p2 remain of
                    Nothing -> Nothing
                    Just (res, finalRemain) -> Just (f res, finalRemain)))

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'
-- | abParser
-- >>> runParser abParser "abcdef" == Just (('a', 'b'), "cdef")
-- True
-- >>> runParser abParser "aebcdf" == Nothing
-- True

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\x y -> [x, y]) <$> posInt <*  char ' ' <*> posInt
-- | intPair
-- >>> runParser intPair "12 34" == Just ([12, 34], "")
-- True

instance Alternative Parser where
    empty = Parser (const Nothing)
    p1 <|> p2 = Parser (\s -> runParser p1 s <|> runParser p2 s)
    
intOrUppercase :: Parser ()
intOrUppercase = () <$ satisfy isUpper <|> () <$ posInt
-- | intOrUppercase
-- >>> runParser intOrUppercase "342abcd" == Just ((), "abcd")
-- True

-- >>> runParser intOrUppercase "XYZ" == Just ((), "YZ")
-- True

-- >>> runParser intOrUppercase "foo" == Nothing
-- True
