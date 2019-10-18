{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Solvers.Minimax 
  ( minimax, minimaxAB, mtdf--, mtdfMem
  ) where

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
{-
data NodeFlag = Upper | Lower | ExactBound
isUpper :: NodeFlag -> Bool
isUpper Upper = True 
isUpper _ = False
isLower :: NodeFlag -> Bool
isLower Lower = True 
isLower _ = False
isExact :: NodeFlag -> Bool
isExact ExactBound = True 
isExact _ = False

data ABCacheNode b = ABCacheNode
  { flag :: NodeFlag
  , storeDepth :: Int
  , value :: b
  }

minimaxABCached :: Solver a b -> Bool -> ExtendedNum Int -> ExtendedNum Int -> Int -> a -> State (M.IntMap (ABCacheNode b)) (SolverResult b)
minimaxABCached s@Solver{..} maxPlayer a b depth game = do 
  let hash = generateHash game
  cache <- get 
  let mCached = M.lookup hash cache
  case mCached of 
    Just ABCacheNode{..} -> 
      if storeDepth >= depth then 
        if isExact flag then return $ buildNode (getScore value) game
        else 
          let 
            newAlpha = if isLower flag then max a value else a 
            newBeta = if isUpper flag then min b value else b
          in 
            doCache
      else doCache 
     doCache
  where 
    doCache a' b' = 
      let 
        result = minimaxABMem' s maxPlayer a' b' depth game 
      in
        modify (M.insert hash (makeResult result)) >> return result
    makeResult result = 
      let 
        resultValue = getScore result
        flag
          | resultValue <= a = Upper
          | resultValue >= b = Lower 
          | otherwise = ExactBound
      in ABCacheNode 
        { flag = flag 
        , storeDepth = depth 
        , value = result 
        }


minimaxABCached' :: Solver a b -> Bool -> ExtendedNum Int -> ExtendedNum Int -> Int -> a -> State (M.IntMap (ABCacheNode b)) (SolverResult b)
minimaxABCached' Solver{..} _ _ _ 0 game = return $ buildNode (Exact $ evaluateScore game) game
minimaxABCached' s@Solver{..} maxPlayer a b depth game 
  | maxPlayer = return $ buildNode (minMaxScore maximum (reverse branches)) game
  | otherwise = return $ buildNode (minMaxScore minimum branches) game
  where 
    branches = minimaxHelper s maxPlayer a b depth (getMoves game) 
    minMaxScore _ [] = Exact $ evaluateScore game
    minMaxScore comp children = (makeScore . unwrapScore) $ comp (map getScore children)
    --minMaxScore comp children = (makeScore . comp) $ map (unwrapScore . getScore) children
    makeScore :: Int -> ABScore Int
    makeScore score 
      | Only score <= a || Only score >= b = Estimate score
      | otherwise = Exact score


minimaxABCachedHelper :: Solver a b -> Bool -> ExtendedNum Int -> ExtendedNum Int -> Int -> [a] -> State (M.IntMap (ABCacheNode b)) [SolverResult b]
minimaxABCachedHelper s maxPlayer a b depth nodes = 
  let startingValue = if maxPlayer then NegInf else PosInf 
  in minimaxABMemHelper' s maxPlayer a b startingValue depth nodes 

minimaxABCachedHelper' :: Solver a b 
                    -> Bool 
                    -> ExtendedNum Int 
                    -> ExtendedNum Int 
                    -> ExtendedNum Int 
                    -> Int 
                    -> [a] 
                    -> State (M.IntMap (ABCacheNode b)) [SolverResult b]
minimaxABCachedHelper' _ _ _ _ _ _ [] = []
minimaxABCachedHelper' s@Solver{..} maxPlayer alpha beta value depth (x : xs)
  | alpha >= beta = []
  | otherwise = 
    let 
      comp = if maxPlayer then max else min 
      newNode = minimaxABMemCache s (nextPlayer x maxPlayer) alpha beta (depth - 1) x 
      newValue = comp value (Only $ (unwrapScore . getScore) newNode)
      newA = newAlpha alpha newValue 
      newB = newBeta beta newValue 
    in 
      newNode : minimaxHelper' s maxPlayer newA newB newValue depth xs
    where 
      newAlpha alpha' v = if maxPlayer then max alpha' v else alpha'
      newBeta beta' v = if not maxPlayer then max beta' v else beta'
      -}
{-
data ABCacheNode b = ABCacheNode
  { upperBound :: ExtendedNum Int
  , lowerBound :: ExtendedNum Int 
  , storeDepth :: Int
  , value :: b
  }

data ABState b = ABState 
  { alpha :: ExtendedNum Int 
  , beta :: ExtendedNum Int 
  , cache :: M.IntMap (ABCacheNode b)}

type ABState b = State (MTDFState b (ABCacheNode b)) (SolverResult b)
-}
{-
minimaxABMem :: Solver a b -> Bool -> Int -> a -> ABState b
minimaxABMem s@Solver{..} maxPlayer depth game = do
  let hash = generateHash game
  state@ABState{..} <- get
  let mCached = M.lookup hash cache
  case mCached of 
    Just x 
      | lowerBound >= beta && storeDepth >= depth -> return $ buildNode lowerBound value
      | upperBound <= alpha && storeDepth >= depth -> return $ buildNode upperBound value
      | otherwise -> do 
          put $ state 
            { alpha = max alpha lowerBound 
            , beta = min beta upperBound 
            }
            -- make call here
    otherwise -> 
      --make call here

minimaxABMem' :: Solver a b -> Bool -> Int -> a -> ABState b
minimaxABMem' Solver{..} _ 0 game = return $ buildNode (Exact $ evaluateScore game) game
minimaxABMem' s@Solver{..} maxPlayer depth game = 
  | maxPlayer = return $ buildNode (minMaxScore maximum (reverse branches)) game
  | otherwise = return $ buildNode (minMaxScore minimum brances) game
  where 
    branches = evalState (minimaxHelperMem s maxPlayer depth (getMoves game)) 
    minMaxScore _ [] = Exact $ evaluateScore game 
    minMaxScore comp children = (makeScore . unwrapScore) $ comp (map getScore children)
    makeScore score 
      | Only score <= a || Only score >= b = Estimate score 
      | otherwise = Exact score

minimaxHelperMem :: Solver a b -> Bool -> Int -> [a] -> 
  
doCache :: (MonadState (Map a b) m, Ord a) => (a -> b) -> a -> m b 
doCache f x = do 
  mCached <- gets (M.lookup x)
  case mCached of 
    Nothing -> modify (M.insert x result) >> return result
    Just cached -> return cached 
  where 
    result = f x

  -}
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