{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Solvers.Types 
  ( ABScore(..), unwrapScore
  , Solver(..)
  , Options(..), emptyOptions, getSelectedSolver
  , SolverFunc, SolverResult, getScore
  , Msg(..), Interface(..)
  , mSolverDropDown, mSearchDepth
  ) where

import Miso.String (fromMisoString, MisoString)
import Control.Lens (makeLenses)
import Data.Tree

import qualified Shared.DropDown

data Msg 
  = DropDown Shared.DropDown.Msg
  | UpdateDepth MisoString
  deriving Show

data Interface action = Interface 
  { passAction :: Msg -> action 
  }

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
  { evaluateScore :: a -> Int
  , buildNode :: ABScore Int -> a -> SolverResult b
  , getMoves :: a -> [a]
  , nextPlayer :: a -> Bool -> Bool
  , showNode :: b -> String
  , generateHash :: a -> Int
  }

type SolverFunc a b = Solver a b -> Bool -> Int -> a -> [SolverResult b]
type SolverResult b = (ABScore Int, b)

getScore :: SolverResult b -> ABScore Int
getScore = fst

data Options = Options
  { _mSolverDropDown :: Shared.DropDown.Model
  , _mSearchDepth :: Int 
  } deriving Eq

makeLenses ''Options

getSelectedSolver :: Options -> String 
getSelectedSolver = fromMisoString . Shared.DropDown._mSelected . _mSolverDropDown

emptyOptions :: [MisoString] -> Options
emptyOptions solverNames = 
  Options 
    { _mSolverDropDown = Shared.DropDown.Model 
      { Shared.DropDown._mTitle = "Algorithm"
      , Shared.DropDown._mSelected = head solverNames
      , Shared.DropDown._mOptions = solverNames 
      , Shared.DropDown._mExpanded = False
      }
    , _mSearchDepth = 6
    }

unwrapScore :: ABScore a -> a 
unwrapScore (Estimate a) = a
unwrapScore (Exact a) = a

