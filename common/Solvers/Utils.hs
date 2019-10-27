module Solvers.Utils
  ( runSolver
  ) where

import Data.Ord (comparing)
import Data.List (maximumBy, minimumBy)

import Solvers.Types

runSolver :: SolverFunc a b -> Solver a b -> Bool -> Int -> a -> SolverResult b 
runSolver strategy solver maxPlayer depth game = 
  let
    moves = strategy solver maxPlayer depth game 
    move = getOptimalMove moves
  in 
    --trace ("Found one: " ++ (showNode solver . snd $ move)
    --                     ++ " => "
    --                     ++ (show . getScore $ move))
                         move
  where 
    getOptimalMove nodes = 
      if maxPlayer then maximumBy (comparing fst) (reverse nodes)
      else minimumBy (comparing fst) nodes
