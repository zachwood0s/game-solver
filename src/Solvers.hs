{-# LANGUAGE OverloadedStrings #-}

module Solvers 
  ( solverMap, runSolverFromString
  ) where

import Data.Map

import Solvers.Minimax
import Solvers.Model
import Solvers.Utils


solverMap :: Map String (SolverFunc a b)
solverMap =
  fromList 
    [ ("Minimax", minimax)
    , ("Minimax w/ Alpha Beta", minimaxAB)
    , ("MTD(f)", mtdf)
    , ("MTD(f) w/ Memory", mtdfMem)
    ]

runSolverFromString :: String -> Solver a b -> Bool -> Int -> a -> SolverResult b
runSolverFromString name s maxPlayer depth game =
  case Data.Map.lookup name solverMap of 
    Just strategy -> runSolver strategy s maxPlayer depth game
    Nothing -> buildNode s (Exact 0) game