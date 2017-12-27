{- CIS 194 HW 10
   due Monday, 1 April
-}

module Week10 where

import           Control.Applicative

import           Data.Char

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


first :: (a->b) -> (a,c) -> (b,c)
first f (a,c) = (f a,c)

testf :: a -> b
testf = undefined

instance Functor Parser where
--fmap f (Parser rp) = Parser (\x -> fmap (first f) $ rp x)
  fmap f (Parser rp) = Parser (fmap (first f) . rp )

instance Applicative Parser where
  pure a = Parser f
           where f s = Just(a,s)

  p1 <*> p2 = Parser f
    where f s = case runParser p1 s of
                  Nothing -> Nothing
                  Just(g,str) -> first g <$> runParser p2 str
{--
  p1 <*> p2 = Parser f
    where f s = case runParser p1 s of
                  Nothing -> Nothing
                  Just(g,str) -> case runParser p2 str of
                                  Nothing -> Nothing
                                  Just(h,str1) ->Just(g h,str1)
--}


--fmap (+) [1,2] <*> [3,4] = [4,5,5,6]
abParser :: Parser(Char,Char)
abParser = (\x y ->(x,y)) <$> char 'a' <*> char 'b'
--abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = (\x -> ()) <$> abParser
--abParser_ = const () <$> abParser

intPair :: Parser [Integer]
intPair = (\x y z -> [x,z]) <$> posInt <*> char ' ' <*> posInt

instance Alternative Parser where
  --empty = Parser (const Nothing)
  empty = Parser (\x-> Nothing)
  p1 <|> p2 = Parser f where
    f x = case runParser p1 x  of
            Nothing -> runParser p2 x
            otherwise -> runParser p1 x

intOrUppercase :: Parser()
intOrUppercase = const () <$> posInt <|> const () <$> satisfy isUpper

