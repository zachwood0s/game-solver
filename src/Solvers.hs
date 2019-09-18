{-# LANGUAGE OverloadedStrings #-}

module Solvers 
  ( solverMap
  ) where

import Data.Map
import Miso.String

import Solvers.Minimax
import Solvers.Model


solverMap :: Map MisoString (SolverFunc a b)
solverMap =
  fromList 
    [ ("Minimax", minimax)
    , ("Minimax w/ Alpha Beta", minimaxAB)
    ]