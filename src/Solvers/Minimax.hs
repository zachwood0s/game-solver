{-# LANGUAGE RecordWildCards #-}

module Solvers.Minimax 
  ( minimax, minimaxAB
  ) where

import Data.Tree
import Solvers.Model
import ExtendedNum

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