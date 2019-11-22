module Utils where

import Control.Monad
import Control.Applicative

inrange :: (Int, Int) -> Int -> Bool
inrange (bot, top) n = bot <= n && n <= top

fOr :: (a -> Bool)
    -> (a -> Bool)
    -> a -> Bool 
fOr = liftA2 (||)

fAnd :: (a -> Bool)
    -> (a -> Bool)
    -> a -> Bool 
fAnd = liftA2 (&&)

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

toMaybe :: Bool -> a -> Maybe a 
toMaybe p = if p then Just else const Nothing

fToMaybe :: (a -> Bool) -> a -> Maybe a 
fToMaybe p a = toMaybe (p a) a

firstJust :: Maybe a -> Maybe a -> Maybe a 
firstJust a b = firstJusts [a, b]

firstJusts :: [Maybe a] -> Maybe a
firstJusts = msum

if' :: Bool -> Maybe a -> Maybe a 
if' p m = if p then m else Nothing