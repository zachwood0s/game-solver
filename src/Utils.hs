module Utils
  ( inrange
  , fAnd2, fOr2
  , symbol, lexeme
  , mapInd
  ) where

import Text.ParserCombinators.Parsec

inrange :: (Int, Int) -> Int -> Bool
inrange (bot, top) n = bot <= n && n <= top

fAnd2 :: (a -> b -> Bool) 
      -> (a -> b -> Bool) 
      -> a -> b -> Bool
fAnd2 f1 f2 a b = f1 a b && f2 a b

fOr2 :: (a -> b -> Bool) 
     -> (a -> b -> Bool) 
     -> a -> b -> Bool
fOr2 f1 f2 a b = f1 a b || f2 a b


-- Parser Helpers
symbol :: Char -> Parser Char
symbol s = lexeme (char s)
lexeme :: Parser a -> Parser a
lexeme parser = parser <* spaces


mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]