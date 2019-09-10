
import Data.Tree
import Test.Tasty
import Test.Tasty.HUnit

import Solver (Solver(..), minimax, minimaxAB)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [solverTests]

solverTests = testGroup "Solver Tests" 
  [ simpleMinimax
  , simpleMinimax2
  , alphaBetaMinimax
  , alphaBetaMinimax2
  ]

simpleMinimax = testCase "Creates a simple minimax tree correctly" $
  minimax intSolver True testTree1 @?= resultTree1

simpleMinimax2 = testCase "Creates a simple minimax tree correctly" $
  minimax intSolver True testTree2 @?= resultTree2

alphaBetaMinimax = testCase "Creates a alpha beta pruned minimax tree correctly" $
  minimaxAB intSolver True testTree1 @?= prunedResultTree1

alphaBetaMinimax2 = testCase "Creates a alpha beta pruned minimax tree correctly" $
  minimaxAB intSolver True testTree2 @?= prunedResultTree2


intSolver = Solver 
  { getScore = id 
  , evaluateScore = id
  , buildNode = const 
  }

{--------------
Test Trees
---------------}

testTree1 :: Tree Int
testTree1 = 
  Node 0 
  [ Node 0 
    [ Node 0 
      [ Node 0 
        [ Node 3 []
        , Node 17 []
        ]
      , Node 0 
        [ Node 2 []
        , Node 12 []
        ]
      ]
    , Node 0 
      [ Node 0 
        [ Node 15 [] ]
      , Node 0
        [ Node 25 [] 
        , Node 0 []
        ]
      ]
    ]
  , Node 0 
    [ Node 0
      [ Node 0
        [ Node 2 []
        , Node 5 []
        ]
      , Node 0 
        [ Node 3 []
        ]
      ]
    , Node 0
      [ Node 0 
        [ Node 2 []
        , Node 14 []
        ]
      ]
    ]
  ]

resultTree1 :: Tree Int
resultTree1 = 
  Node 3 
  [ Node 3 
    [ Node 3 
      [ Node 3 
        [ Node 3 []
        , Node 17 []
        ]
      , Node 2 
        [ Node 2 []
        , Node 12 []
        ]
      ]
    , Node 15
      [ Node 15
        [ Node 15 [] ]
      , Node 0
        [ Node 25 [] 
        , Node 0 []
        ]
      ]
    ]
  , Node 2
    [ Node 3
      [ Node 2
        [ Node 2 []
        , Node 5 []
        ]
      , Node 3
        [ Node 3 []
        ]
      ]
    , Node 2
      [ Node 2
        [ Node 2 []
        , Node 14 []
        ]
      ]
    ]
  ]

prunedResultTree1 :: Tree Int
prunedResultTree1 =
  Node 3 
  [ Node 3 
    [ Node 3 
      [ Node 3 
        [ Node 3 []
        , Node 17 []
        ]
      , Node 2 
        [ Node 2 []
        ]
      ]
    , Node 15
      [ Node 15
        [ Node 15 [] ]
      ]
    ]
  , Node 3
    [ Node 3
      [ Node 2
        [ Node 2 []
        ]
      , Node 3
        [ Node 3 []
        ]
      ]
    ]
  ]

testTree2 :: Tree Int
testTree2 =
  Node 0 
  [ Node 0
    [ Node 0
      [ Node 0 
        [ Node 5 []
        , Node 6 []
        ]
      , Node 0
        [ Node 7 []
        , Node 4 []
        , Node 5 []
        ]
      ]
    , Node 0
      [ Node 0 
        [ Node 3 []
        ]
      ]
    ]
  , Node 0 
    [ Node 0 
      [ Node 0 
        [ Node 6 []
        ]
      , Node 0 
        [ Node 6 []
        , Node 9 []
        ]
      ]
    , Node 0 
      [ Node 0 
        [ Node 7 []
        ]
      ]
    ]
  , Node 0 
    [ Node 0 
      [ Node 0 
        [ Node 5 [] 
        ]
      ]
    , Node 0 
      [ Node 0 
        [ Node 9 []
        , Node 8 []
        ]
      , Node 0 
        [ Node 6 []
        ]
      ]
    ]
  ]

resultTree2 :: Tree Int
resultTree2 =
  Node 6 
  [ Node 3
    [ Node 5
      [ Node 5 
        [ Node 5 []
        , Node 6 []
        ]
      , Node 4
        [ Node 7 []
        , Node 4 []
        , Node 5 []
        ]
      ]
    , Node 3
      [ Node 3 
        [ Node 3 []
        ]
      ]
    ]
  , Node 6 
    [ Node 6 
      [ Node 6 
        [ Node 6 []
        ]
      , Node 6 
        [ Node 6 []
        , Node 9 []
        ]
      ]
    , Node 7 
      [ Node 7 
        [ Node 7 []
        ]
      ]
    ]
  , Node 5 
    [ Node 5 
      [ Node 5 
        [ Node 5 [] 
        ]
      ]
    , Node 8 
      [ Node 8 
        [ Node 9 []
        , Node 8 []
        ]
      , Node 6 
        [ Node 6 []
        ]
      ]
    ]
  ]

prunedResultTree2 :: Tree Int
prunedResultTree2 =
  Node 6 
  [ Node 3
    [ Node 5
      [ Node 5 
        [ Node 5 []
        , Node 6 []
        ]
      , Node 4
        [ Node 7 []
        , Node 4 []
        ]
      ]
    , Node 3
      [ Node 3 
        [ Node 3 []
        ]
      ]
    ]
  , Node 6 
    [ Node 6 
      [ Node 6 
        [ Node 6 []
        ]
      , Node 6 
        [ Node 6 []
        ]
      ]
    , Node 7 
      [ Node 7 
        [ Node 7 []
        ]
      ]
    ]
  , Node 5 
    [ Node 5 
      [ Node 5 
        [ Node 5 [] 
        ]
      ]
    ]
  ]

