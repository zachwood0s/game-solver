{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Solvers.MinimaxCached 
  ( minimaxCached, minimaxCachedM
  ) where

import qualified Data.IntMap.Strict as M
import Control.Monad.State.Lazy

import Solvers.Utils
import Solvers.Model
import ExtendedNum

data MinimaxCacheNode = MinimaxCacheNode
  { storeDepth :: Int 
  , value :: ABScore Int
  }

type MinimaxCache = M.IntMap MinimaxCacheNode

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
    Just cached -> return $ buildNode (value cached) game
  where 
    hash = generateHash game
    isValid cached
      | storeDepth cached < depth = Nothing 
      | otherwise = Just cached
    doCache = do 
      result <- minimaxCachedM' s maxPlayer depth game
      modify (M.insert hash (makeCache result))
      return result 
      where 
        makeCache res = MinimaxCacheNode depth (getScore res)

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
        getNewScore children = comparison $ map getScore children