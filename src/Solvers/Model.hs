{-# LANGUAGE OverloadedStrings #-}

module Solvers.Model 
  ( ABScore(..), unwrapScore
  , Solver(..)
  , SolverFunc, Options(..), emptyOptions, getSelectedSolver
  ) where

import Data.Tree
import Miso.String (MisoString)
import Shared.DropDown (Model(..))

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

data Options = Options
  { solverDropDown :: Shared.DropDown.Model
  , searchDepth :: Int 
  } deriving Eq

getSelectedSolver :: Options -> MisoString 
getSelectedSolver = selected . solverDropDown

emptyOptions :: [MisoString] -> Options
emptyOptions solverNames = 
  Options 
    { solverDropDown = Shared.DropDown.Model 
      { title = "Algorithm"
      , selected = head solverNames
      , options = solverNames 
      , expanded = False
      }
    , searchDepth = 4
    }

unwrapScore :: ABScore a -> a 
unwrapScore (Estimate a) = a
unwrapScore (Exact a) = a