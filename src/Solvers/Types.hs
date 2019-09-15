module Solvers.Types 
  ( ABScore(..), unwrapScore
  , Solver(..)
  , SolverFunc, SolverOptions(..)
  ) where

import Data.Tree

data ABScore a = Estimate a | Exact a deriving (Eq)

instance (Show a) =>  Show (ABScore a) where
  show (Estimate a) = "~" ++ show a
  show (Exact a) = show a

instance (Num a, Eq a) => Num (ABScore a) where
  Estimate a + Estimate b = Estimate (a + b)
  Estimate a + Exact b = Estimate (a + b)
  Exact a + Exact b = Exact (a + b)
  x + y = y + x
  x - y = x + Exact (-1) * y

  Exact a * Exact b = Exact (a * b)
  Estimate a * Estimate b = Estimate (a * b)
  Estimate a * Exact b = Estimate (a * b)
  x * y = y * x

  abs (Estimate a) = Estimate (abs a)
  abs (Exact a) = Exact (abs a)
  signum (Exact a) = Exact (signum a)
  signum (Estimate a) = Estimate (signum a)

  fromInteger a = Exact $ fromInteger a

instance (Ord a) => Ord (ABScore a) where 
  Estimate a <= Estimate b = a <= b
  Estimate a <= Exact b = a <= b
  Exact a <= Estimate b = a <= b
  Exact a <= Exact b = a <= b


data Solver a b = Solver 
  { getScore :: b -> ABScore Int
  , evaluateScore :: a -> Int
  , buildNode :: ABScore Int -> a -> b
  }

type SolverFunc a b = Solver a b -> Bool -> Tree a -> Tree b

data SolverOptions a b = SolverOptions
  { selectedSolverFunc :: (String, SolverFunc a b)
  , solverList :: [(String, SolverFunc a b)]
  , searchDepth :: Int 
  } 

instance Eq (SolverOptions a b) where
  (SolverOptions sel1 list1 depth1) == (SolverOptions sel2 list2 depth2) = 
       depth1 == depth2 
    && map fst list1 == map fst list2
    && fst sel1 == fst sel2

unwrapScore :: ABScore a -> a 
unwrapScore (Estimate a) = a
unwrapScore (Exact a) = a