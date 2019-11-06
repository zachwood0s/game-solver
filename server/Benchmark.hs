import Criterion.Main
import qualified Data.Map as M

import Solvers 
import Solvers.Types (Solver)
import qualified Games.TicTacToe as TicTacToe

-- Our benchmark harness.
main = 
  let 
    smallBoard = ((-1, -1), TicTacToe.emptyModel (3, 3) 3)
    mediumBoard = ((-1, -1), TicTacToe.emptyModel (5, 6) 4)
    largeBoard = ((-1, -1), TicTacToe.emptyModel (10, 10) 5)
  in
    defaultMain 
      [ bgroup "Small" 
        [ searchDepth TicTacToe.solver smallBoard 8
        , searchDepth TicTacToe.solver smallBoard 9
        ]
      , bgroup "Medium"
        [ searchDepth TicTacToe.solver mediumBoard 3
        , searchDepth TicTacToe.solver mediumBoard 4
        ]
      , bgroup "Large"
        [ searchDepth TicTacToe.solver largeBoard 3
        ]
      ]

searchDepth :: Solver a b -> a -> Int -> Benchmark 
searchDepth solver game depth = 
  bgroup ("Depth" ++ show depth) solve
  where
    solve = map (solverBench solver game depth) (M.keys solverMap)



solverBench :: Solver a b -> a -> Int -> String -> Benchmark 
solverBench solver game depth name =
  bench name $ whnf go game
  where 
    go = runSolverFromString name solver True depth