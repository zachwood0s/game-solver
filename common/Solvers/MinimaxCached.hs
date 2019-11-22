{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell #-}

module Solvers.MinimaxCached 
  ( minimaxCached, minimaxCachedM
  , minimaxABCached, minimaxABCachedM
  ) where

import qualified Data.IntMap.Strict as M
import Control.Monad.State.Lazy
import Control.Lens

import Solvers.Types
import ExtendedNum

-- Minimax Cache Node
data Mcn = Mcn
  { mcnStoreDepth :: Int 
  , mcnValue :: ABScore Int
  }

makeFields ''Mcn

type MinimaxCache = M.IntMap Mcn

minimaxCached :: Solver a b -> Bool -> Int -> a -> [SolverResult b]
minimaxCached s maxPlayer depth game =
  evalState (minimaxCachedM s maxPlayer depth game) M.empty

minimaxCachedM :: MonadState MinimaxCache m 
               => Solver a b 
               -> Bool 
               -> Int 
               -> a 
               -> m [SolverResult b]
minimaxCachedM s@Solver{..} maxPlayer depth game = 
  mapM (minimaxDoCacheM s maxPlayer (depth - 1)) (getMoves game)

minimaxDoCacheM :: MonadState MinimaxCache m 
                => Solver a b 
                -> Bool 
                -> Int 
                -> a 
                -> m (SolverResult b)
minimaxDoCacheM s@Solver{..} maxPlayer depth game = do 
  mCached <- gets ( maybe Nothing isValid . M.lookup hash)
  case mCached of 
    Nothing -> doCache
    Just cached -> return $ buildNode (cached ^. value) game
  where 
    hash = generateHash game
    isValid cached
      | cached ^. storeDepth < depth = Nothing 
      | otherwise = Just cached
    doCache = do 
      result <- minimaxCachedM' s maxPlayer depth game
      modify (M.insert hash (makeCache result))
      return result 
      where 
        makeCache res = Mcn depth (getScore res)

minimaxCachedM' :: MonadState MinimaxCache m 
                => Solver a b 
                -> Bool 
                -> Int 
                -> a 
                -> m (SolverResult b)
minimaxCachedM' Solver{..} _ 0 game = return $ buildNode (Exact $ evaluateScore game) game
minimaxCachedM' s@Solver{..} maxPlayer depth game 
  | maxPlayer = bestBranch maximum 
  | otherwise = bestBranch minimum
  where 
    bestBranch comparison = do 
      branches <- mapM go (getMoves game)
      return $ buildNode (getNewScore branches) game
      where 
        go = minimaxDoCacheM s (nextPlayer game maxPlayer) (depth - 1) 
        getNewScore [] = Exact $ evaluateScore game 
        getNewScore xs = comparison $ map getScore xs



data Abcn = Abcn
  { abcnStoreDepth :: Int 
  , abcnLowerBound :: ExtendedNum Int 
  , abcnUpperBound :: ExtendedNum Int
  }

makeFields ''Abcn

type AlphaBetaCache = M.IntMap Abcn

  
minimaxABCached :: Solver a b -> Bool -> Int -> a -> [SolverResult b]
minimaxABCached s maxPlayer depth game = 
  evalState (minimaxABCachedM s maxPlayer NegInf PosInf depth game) M.empty

minimaxABCachedM :: MonadState AlphaBetaCache m 
                 => Solver a b 
                 -> Bool 
                 -> ExtendedNum Int 
                 -> ExtendedNum Int
                 -> Int 
                 -> a 
                 -> m [SolverResult b]
minimaxABCachedM s@Solver{..} maxPlayer a b depth game = 
  abCachedHelperM s maxPlayer a b (depth - 1) (getMoves game)

minimaxABCachedM' :: MonadState AlphaBetaCache m 
                  => Solver a b 
                  -> Bool 
                  -> ExtendedNum Int 
                  -> ExtendedNum Int 
                  -> Int 
                  -> a 
                  -> m (SolverResult b)
minimaxABCachedM' Solver{..} _ _ _ 0 game = return $ buildNode (Exact $ evaluateScore game) game
minimaxABCachedM' s@Solver{..} maxPlayer a b depth game 
  | maxPlayer = bestBranch maximum reverse 
  | otherwise = bestBranch minimum id --No transform needed
  where 
    --bestBranch :: ([ABScore Int] -> ABScore Int) -> j -> m (SolverResult b)
    bestBranch comparison trans = do
      branches <- abCachedHelperM s maxPlayer a b depth (getMoves game)
      return $ buildNode (getNewScore $ trans branches) game
      where 
        getNewScore [] = Exact $ evaluateScore game 
        getNewScore xs = (makeScore . unwrapScore) $ comparison (map getScore xs)
        makeScore score 
          | Only score <= a || Only score >= b = Estimate score 
          | otherwise = Exact score


abCachedHelperM :: MonadState AlphaBetaCache m 
                => Solver a b 
                -> Bool 
                -> ExtendedNum Int 
                -> ExtendedNum Int 
                -> Int 
                -> [a]
                -> m [SolverResult b]
abCachedHelperM s maxPlayer a b depth nodes =
  let startingValue = if maxPlayer then NegInf else PosInf 
  in abCachedHelperM' s maxPlayer a b startingValue depth nodes

abCachedHelperM' :: MonadState AlphaBetaCache m 
                 => Solver a b 
                 -> Bool 
                 -> ExtendedNum Int 
                 -> ExtendedNum Int 
                 -> ExtendedNum Int 
                 -> Int 
                 -> [a]
                 -> m [SolverResult b]
abCachedHelperM' _ _ _ _ _ _ [] = return []
abCachedHelperM' s@Solver{..} maxPlayer alpha beta val depth (x : xs)
  | alpha >= beta = return []
  | maxPlayer = go max 
  | otherwise = go min
  where 
    go comparison = do 
      newNode <- abDoCacheM s (nextPlayer x maxPlayer) alpha beta (depth - 1) x
      let v = getNewValue newNode
      r <- abCachedHelperM' s maxPlayer (newAlpha v) (newBeta v) v depth xs 
      return $ newNode : r
      where 
        getNewValue node = comparison val (Only $ (unwrapScore . getScore) node)
        newAlpha v = if maxPlayer then max alpha v else alpha
        newBeta v = if not maxPlayer then max beta v else beta

abDoCacheM :: MonadState AlphaBetaCache m 
           => Solver a b 
           -> Bool 
           -> ExtendedNum Int 
           -> ExtendedNum Int 
           -> Int 
           -> a 
           -> m (SolverResult b)
abDoCacheM s@Solver{..} maxPlayer alpha beta depth game = do 
  ((a', b'), mCached) <- gets (maybe default_ isValid . M.lookup hash)
  case mCached of 
    Nothing -> doCache a' b'
    Just cached -> return $ buildNode (Estimate $ extendedNum2Num cached) game
  where 
    default_ = ((alpha, beta), Nothing)
    hash = generateHash game 
    isValid :: Abcn -> ((ExtendedNum Int, ExtendedNum Int), Maybe (ExtendedNum Int))
    isValid cached 
      | cached ^. storeDepth < depth =  default_
      | cached ^. lowerBound >= beta = ((alpha, beta), Just (cached ^. lowerBound))
      | cached ^. upperBound <= alpha = ((alpha, beta), Just (cached ^. upperBound))
      | otherwise = 
        let 
          a' = max alpha (cached ^. lowerBound)
          b' = min beta (cached ^. upperBound)
        in ((a', b'), Nothing)
    doCache a' b' = do 
      result <- minimaxABCachedM' s maxPlayer a' b' depth game 
      modify (M.insert hash (makeCache $ (Only . unwrapScore . getScore) result))
      return result 
      where 
        makeCache score = 
          let 
            (lower, upper)
              | score <= a' = (NegInf, score)
              | score >= b' = (score, PosInf)
              | otherwise = (score, score)
          in 
            Abcn depth lower upper

{-

mtdfMem :: Solver a b -> Bool -> Int -> a -> [SolverResult b]
mtdfMem s@Solver{..} maxPlayer depth game = 
  let 
    startGuess = buildNode (Exact 500) game
    res = evalState (mtdfMem' s maxPlayer PosInf NegInf depth startGuess game) startState
  in [res]

mtdfMem' :: MonadState AlphaBetaCache m 
         => Solver a b 
         -> Bool 
         -> ExtendedNum Int 
         -> ExtendedNum Int
         -> Int 
         -> SolverResult b
         -> a 
         -> m (SolverResult b)
mtdfMem' s@Solver{..} maxPlayer upper lower depth startGuess game = do 
  state@MTDFState{..} <- get
  if lowerBound >= upperBound then return currentGuess
  else do
    let extendedGuess = makeExtended currentGuess 
    let beta = max extendedGuess (lowerBound + 1)
    let newGuess = runSolver (go beta) s maxPlayer depth game
    let newExtended = makeExtended newGuess 
    let (upper', lower') = if newExtended < beta then (newExtended, lowerBound) 
                           else (upperBound, newExtended)
    mtdfMem' s maxPlayer depth game
  where 
    makeExtended = Only . extractGuess . getScore 
    extractGuess (Exact a) = a 
    extractGuess (Estimate a) = a
    go beta s m d g = minimaxHelper s m (beta - 1) beta d (getMoves g)
    -}