{-# LANGUAGE RecordWildCards #-}

module Solvers.Minimax 
  ( minimax, minimaxAB, mtdf, mtdfMem
  ) where

import Debug.Trace
import Data.List
import Data.Ord
import Data.Bifunctor
import qualified Data.IntMap.Strict as M
import Control.Monad.State.Lazy

import Solvers.Utils
import Solvers.Model
import ExtendedNum

minimax :: Solver a b -> Bool -> Int -> a -> [SolverResult b]
minimax s@Solver{..} maxPlayer depth game =
  map (minimax' s maxPlayer (depth - 1)) (getMoves game)

minimax' :: Solver a b -> Bool -> Int -> a -> SolverResult b
minimax' Solver{..} _ 0 game = buildNode (Exact $ evaluateScore game) game
minimax' s@Solver{..} maxPlayer depth game 
  | maxPlayer = buildNode (getNewScore maximum branches) game
  | otherwise = buildNode (getNewScore minimum branches) game
  where 
    branches = map (minimax' s (nextPlayer game maxPlayer) (depth - 1)) (getMoves game)
    getNewScore _ [] = Exact $ evaluateScore game
    getNewScore comp children = comp $ map getScore children

minimaxAB :: Solver a b -> Bool -> Int -> a -> [SolverResult b]
minimaxAB s@Solver{..} maxPlayer depth game =
  minimaxHelper s maxPlayer NegInf PosInf (depth - 1) (getMoves game)

minimaxAB' :: Solver a b -> Bool -> ExtendedNum Int -> ExtendedNum Int -> Int -> a -> SolverResult b
minimaxAB' Solver{..} _ _ _ 0 game = buildNode (Exact $ evaluateScore game) game
minimaxAB' s@Solver{..} maxPlayer a b depth game 
  | maxPlayer = buildNode (minMaxScore maximum (reverse branches)) game
  | otherwise = buildNode (minMaxScore minimum branches) game
  where 
    branches = minimaxHelper s maxPlayer a b depth (getMoves game) 
    minMaxScore _ [] = Exact $ evaluateScore game
    minMaxScore comp children = (makeScore . unwrapScore) $ comp (map getScore children)
    --minMaxScore comp children = (makeScore . comp) $ map (unwrapScore . getScore) children
    makeScore :: Int -> ABScore Int
    makeScore score 
      | Only score <= a || Only score >= b = Estimate score
      | otherwise = Exact score

minimaxHelper :: Solver a b -> Bool -> ExtendedNum Int -> ExtendedNum Int -> Int -> [a] -> [SolverResult b]
minimaxHelper s maxPlayer a b depth nodes =
  let startingValue = if maxPlayer then NegInf else PosInf
  in minimaxHelper' s maxPlayer a b startingValue depth nodes

minimaxHelper' :: Solver a b -> Bool -> ExtendedNum Int -> ExtendedNum Int -> ExtendedNum Int -> Int -> [a] -> [SolverResult b]
minimaxHelper' _ _ _ _ _ _ [] = []
minimaxHelper' s@Solver{..} maxPlayer alpha beta value depth (x : xs)
  | alpha >= beta = []
  | otherwise = 
    let 
      comp = if maxPlayer then max else min 
      newNode = minimaxAB' s (nextPlayer x maxPlayer) alpha beta (depth - 1) x 
      newValue = comp value (Only $ (unwrapScore . getScore) newNode)
      newA = newAlpha alpha newValue 
      newB = newBeta beta newValue 
    in 
      newNode : minimaxHelper' s maxPlayer newA newB newValue depth xs
    where 
      newAlpha alpha' v = if maxPlayer then max alpha' v else alpha'
      newBeta beta' v = if not maxPlayer then max beta' v else beta'

mtdf :: Solver a b -> Bool -> Int -> a -> [SolverResult b]
mtdf s@Solver{..} maxPlayer depth game =
  [mtdf' s maxPlayer depth (buildNode (Exact 500) game) PosInf NegInf game]

mtdf' :: Solver a b -> Bool -> Int -> SolverResult b -> ExtendedNum Int -> ExtendedNum Int -> a -> SolverResult b
mtdf' s@Solver{..} maxPlayer depth guess upperBound lowerBound game
  | lowerBound >= upperBound = guess --trace "DONE!" guess 
  | otherwise = 
      let 
        extendedGuess = makeExtended guess
        --extendedGuess = trace ("Begin: " ++ show (upperBound, lowerBound, makeExtended guess)) makeExtended guess
        beta = max extendedGuess (lowerBound + 1)
        newGuess = runSolver (go beta) s maxPlayer depth game
        --newGuess = trace ("Beta: "++show beta) minimaxAB' s maxPlayer (beta - 1) beta depth game
        newExtended = makeExtended newGuess
        --newExtended = trace ("new: "++show (makeExtended newGuess)) makeExtended newGuess
        (newUpper, newLower) = 
          if newExtended < beta then (newExtended, lowerBound) 
          else (upperBound, newExtended)
      in 
        --trace ("NewBound: "++ show (newUpper, newLower))
          mtdf' s maxPlayer depth newGuess newUpper newLower game
      where
        makeExtended = Only . extractGuess . getScore 
        extractGuess (Exact a) = a 
        extractGuess (Estimate a) = a
        go beta s m d g = minimaxHelper s m (beta - 1) beta d (getMoves g)


data MTDFCacheNode b = MTDFCacheNode 
  { depth :: Int 
  , value :: b
  }

data MTDFState b = MTDFState 
  { upperBound :: ExtendedNum Int 
  , lowerBound :: ExtendedNum Int 
  , currentGuess :: SolverResult b
  , cache :: M.IntMap (MTDFCacheNode b)
  }

mtdfMem :: Solver a b -> Bool -> Int -> a -> [SolverResult b]
mtdfMem s@Solver{..} maxPlayer depth game = 
  let 
    startState = MTDFState 
      { upperBound = PosInf
      , lowerBound = NegInf 
      , currentGuess = buildNode (Exact 500) game
      , cache = M.empty
      }
    res = evalState (mtdfMem' s maxPlayer depth game) startState
  in [res]

mtdfMem' :: Solver a b -> Bool -> Int -> a -> State (MTDFState b) (SolverResult b)
mtdfMem' s@Solver{..} maxPlayer depth game = do 
  state@MTDFState{..} <- get
  if lowerBound >= upperBound then return currentGuess
  else do
    let extendedGuess = makeExtended currentGuess 
    let beta = max extendedGuess (lowerBound + 1)
    let newGuess = runSolver (go beta) s maxPlayer depth game
    let newExtended = makeExtended newGuess 
    put $ state 
      { upperBound = if newExtended < beta then newExtended else upperBound
      , lowerBound = if newExtended < beta then lowerBound else newExtended
      , currentGuess = newGuess
      }
    mtdfMem' s maxPlayer depth game
  where 
    makeExtended = Only . extractGuess . getScore 
    extractGuess (Exact a) = a 
    extractGuess (Estimate a) = a
    go beta s m d g = minimaxHelper s m (beta - 1) beta d (getMoves g)


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
    minMaxScore comp = makeScore . comp) $ map (unwrapScore . getScore . rootLabel ) children
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