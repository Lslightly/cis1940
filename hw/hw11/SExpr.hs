{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char (isUpper, isSpace, isAlpha, isAlphaNum)

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

-- | zeroOrMore, oneOrMore
-- >>> runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH"
-- Just ("ABC","dEfgH")

-- >>> runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH"
-- Just ("ABC","dEfgH")


------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

-- | ident
-- >>> runParser ident "foobar baz" == Just ("foobar"," baz")
-- >>> runParser ident "foo33fA" == Just ("foo33fA","")
-- >>> runParser ident "" == Nothing
-- True
-- True
-- True

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseSExpr :: Parser SExpr
parseAtom :: Parser Atom
parseInt :: Parser Integer
parseComb :: Parser [SExpr]
parseInt = posInt
parseAtom = spaces *> (
  (N <$> parseInt) <|>
  (I <$> ident)) <* spaces
parseSExpr = spaces *> (
  (A <$> parseAtom) <|>
  (Comb <$> parseComb)) <* spaces
parseComb = spaces *> char '(' *> spaces *> zeroOrMore parseSExpr <* spaces <* char ')' <* spaces

-- | parseSExpr
-- >>> runParser parseSExpr "5"
-- Just (A (N 5),"")

-- >>> runParser parseSExpr "foo3"
-- Just (A (I "foo3"),"")

-- >>> runParser parseSExpr "(bar (foo) 3 5 874)"
-- Just (Comb [A (I "bar"),Comb [A (I "foo")],A (N 3),A (N 5),A (N 874)],"")

-- >>> runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)"
-- Just (Comb [Comb [Comb [A (I "lambda"),A (I "x"),Comb [A (I "lambda"),A (I "y"),Comb [A (I "plus"),A (I "x"),A (I "y")]]],A (N 3)],A (N 5)],"")

-- >>> runParser parseSExpr "(   lots of (  spaces in ) this ( one ) )"
-- Just (Comb [A (I "lots"),A (I "of"),Comb [A (I "spaces"),A (I "in")],A (I "this"),Comb [A (I "one")]],"")
