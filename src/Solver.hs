{-# LANGUAGE RecordWildCards #-}

module Solver 
  ( minimax, minimaxAB 
  , Solver(..) 
  ) 
where
import Data.List
import Data.Ord
import Data.Tree
import Debug.Trace
import ExtendedNum (ExtendedNum(..)) 

data ABScore a = Estimate a | Exact a deriving (Eq, Show)

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


data Solver a b = Solver 
  { getScore :: b -> Int
  , evaluateScore :: a -> Int
  , buildNode :: Int -> a -> b
  }

minimax :: Solver a b -> Bool -> Tree a -> Tree b
minimax Solver{..} _ (Node val []) =  Node (buildNode (evaluateScore val) val) []
minimax s@Solver{..} maximizingPlayer (Node val ts) 
  | maximizingPlayer = Node (buildNode (getNewScore maximum) val) children
  | not maximizingPlayer = Node (buildNode (getNewScore minimum) val) children
  where 
    children = map (minimax s (not maximizingPlayer)) ts
    getNewScore :: ([Int] -> Int) -> Int
    getNewScore comp = comp $ map (getScore . rootLabel) children


minimaxAB :: Solver a b -> Bool -> Tree a -> Tree b 
minimaxAB s maximizingPlayer = minimaxAB' s maximizingPlayer NegInf  PosInf 

minimaxAB' :: Solver a b -> Bool -> ExtendedNum Int -> ExtendedNum Int -> Tree a -> Tree b
minimaxAB' Solver{..} _ _ _ (Node val []) = Node (buildNode (evaluateScore val) val) []
minimaxAB' s@Solver{..} maximizingPlayer a b (Node val ts) 
  | maximizingPlayer = Node (buildNode (minMaxScore maximum) val) children
  | not maximizingPlayer = Node (buildNode (minMaxScore minimum) val) children
  where
    startingValue = if maximizingPlayer then NegInf else PosInf 
    children = minimaxHelper s ts maximizingPlayer a b startingValue
    minMaxScore comp = comp $ map (getScore . rootLabel ) children

minimaxHelper :: Solver a b -> [Tree a] -> Bool -> ExtendedNum Int -> ExtendedNum Int -> ExtendedNum Int -> [Tree b]
minimaxHelper _ [] _ _ _ _ = []
minimaxHelper s@Solver{..} (x : xs) maximizingPlayer alpha beta value
  | alpha >= beta = []
  | otherwise =
    let 
      comp = if maximizingPlayer then max else min 
      newNode = minimaxAB' s (not maximizingPlayer) alpha beta x
      newValue = comp value (Only $ (getScore . rootLabel) newNode)
      newA = newAlpha alpha newValue
      newB = newBeta beta newValue
    in 
      newNode : minimaxHelper s xs maximizingPlayer newA newB newValue
  where
    newAlpha alpha v = 
      if maximizingPlayer then max alpha v
      else alpha
    newBeta beta v = 
      if maximizingPlayer then beta 
      else min beta v