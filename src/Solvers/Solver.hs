{-# LANGUAGE RecordWildCards #-}

module Solver.Solver 
  ( solverList
  , minimax, minimaxAB 
  , SolverFunc, Solver(..), ABScore(..)
  ) 
where
import Data.Tree
import ExtendedNum (ExtendedNum(..)) 

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

type SolverFunc = Solver a b -> Bool -> Tree a -> Tree b

data SolverOptions a b = SolverOptions
  { selectedSolverFunc :: (String, SolverFunc a b)
  , solverList :: [(String, SolverFunc a b)]
  , searchDepth :: Int 
  }

solverList :: [(String, SolverFunc)]
solverList = 
  [ ("MiniMax", minimax)
  , ("MiniMax + Alpha Beta", minimaxAB)
  ]

unwrapScore :: ABScore a -> a 
unwrapScore (Estimate a) = a
unwrapScore (Exact a) = a

minimax :: Solver a b -> Bool -> Tree a -> Tree b
minimax Solver{..} _ (Node val []) =  Node (buildNode (Exact $ evaluateScore val) val) []
minimax s@Solver{..} maximizingPlayer (Node val ts) 
  | maximizingPlayer = Node (buildNode (getNewScore maximum) val) children
  | otherwise  = Node (buildNode (getNewScore minimum) val) children
  where 
    children = map (minimax s (not maximizingPlayer)) ts
    getNewScore comp = Exact $ comp $ map (unwrapScore . getScore . rootLabel) children

minimaxAB :: Solver a b -> Bool -> Tree a -> Tree b 
minimaxAB s maximizingPlayer = minimaxAB' s maximizingPlayer NegInf  PosInf 

minimaxAB' :: Solver a b -> Bool -> ExtendedNum Int -> ExtendedNum Int -> Tree a -> Tree b
minimaxAB' Solver{..} _ _ _ (Node val []) = Node (buildNode (Exact $ evaluateScore val) val) []
minimaxAB' s@Solver{..} maximizingPlayer a b (Node val ts) 
  | maximizingPlayer = Node (buildNode (minMaxScore maximum) val) children
  | otherwise = Node (buildNode (minMaxScore minimum) val) children
  where
    startingValue = if maximizingPlayer then NegInf else PosInf 
    children = minimaxHelper s ts maximizingPlayer a b startingValue
    minMaxScore comp = makeScore $ comp $ map (unwrapScore . getScore . rootLabel ) children
    makeScore score 
      | Only score <= a || Only score >= b = Estimate score
      | otherwise = Exact score

minimaxHelper :: Solver a b -> [Tree a] -> Bool -> ExtendedNum Int -> ExtendedNum Int -> ExtendedNum Int -> [Tree b]
minimaxHelper _ [] _ _ _ _ = []
minimaxHelper s@Solver{..} (x : xs) maximizingPlayer alpha beta value
  | alpha >= beta = []
  | otherwise =
    let 
      comp = if maximizingPlayer then max else min 
      newNode = minimaxAB' s (not maximizingPlayer) alpha beta x
      newValue = comp value (Only $ (unwrapScore . getScore . rootLabel) newNode)
      newA = newAlpha alpha newValue
      newB = newBeta beta newValue
    in 
      newNode : minimaxHelper s xs maximizingPlayer newA newB newValue
  where
    newAlpha alpha' v 
      | maximizingPlayer = max alpha' v
      | otherwise = alpha' 
    newBeta beta' v 
      | maximizingPlayer = beta'
      | otherwise  = min beta' v