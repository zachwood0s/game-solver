{-# LANGUAGE RecordWildCards #-}

module Solvers.Minimax 
  ( minimax, minimaxAB
  ) where

import Debug.Trace

import Solvers.Model
import ExtendedNum

minimax :: Solver a b -> Bool -> Int -> a -> [b]
minimax s@Solver{..} maxPlayer depth game =
  map (minimax' s maxPlayer (depth - 1)) (getMoves game)

minimax' :: Solver a b -> Bool -> Int -> a -> b
minimax' Solver{..} _ 0 game = buildNode (Exact $ evaluateScore game) game
minimax' s@Solver{..} maxPlayer depth game 
  | maxPlayer = buildNode (getNewScore maximum branches) game
  | otherwise = buildNode (getNewScore minimum branches) game
  where 
    branches = map (minimax' s (nextPlayer game maxPlayer) (depth - 1)) (getMoves game)
    getNewScore _ [] = Exact (evaluateScore game)
    getNewScore comp children = (Exact . comp) $ map (unwrapScore . getScore) children

minimaxAB :: Solver a b -> Bool -> Int -> a -> [b]
minimaxAB s@Solver{..} maxPlayer depth game =
  let startingValue = if maxPlayer then NegInf else PosInf
  in minimaxHelper s maxPlayer NegInf PosInf startingValue (depth - 1) (getMoves game)

minimaxAB' :: Solver a b -> Bool -> ExtendedNum Int -> ExtendedNum Int -> Int -> a -> b 
minimaxAB' Solver{..} _ _ _ 0 game = buildNode (Exact $ evaluateScore game) game
minimaxAB' s@Solver{..} maxPlayer a b depth game 
  | maxPlayer = buildNode (minMaxScore maximum branches) game
  | otherwise = buildNode (minMaxScore minimum branches) game
  where 
    startingValue = if maxPlayer then NegInf else PosInf 
    branches = minimaxHelper s maxPlayer a b startingValue depth (getMoves game) 
    minMaxScore _ [] = Exact (evaluateScore game)
    minMaxScore comp children = (makeScore . comp) $ map (unwrapScore . getScore) children
    makeScore score 
      | Only score <= a || Only score >= b = Estimate score 
      | otherwise = Exact score

minimaxHelper :: Solver a b -> Bool -> ExtendedNum Int -> ExtendedNum Int -> ExtendedNum Int -> Int -> [a] -> [b]
minimaxHelper _ _ _ _ _ _ [] = []
minimaxHelper s@Solver{..} maxPlayer alpha beta value depth (x : xs)
  | alpha >= beta = []
  | otherwise = 
    let 
      comp = if maxPlayer then max else min 
      newNode = minimaxAB' s (nextPlayer x maxPlayer) alpha beta (depth - 1) x 
      newValue = comp value (Only $ (unwrapScore . getScore) newNode)
      newA = newAlpha alpha newValue 
      newB = newBeta beta newValue 
    in 
      newNode : minimaxHelper s maxPlayer newA newB newValue depth xs
    where 
      newAlpha alpha' v = if maxPlayer then max alpha' v else alpha'
      newBeta beta' v = if not maxPlayer then max beta' v else beta'
{-
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

-}