module Utils
  ( inrange
  , fAnd2, fOr2
  , mapInd
  ) where


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

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]