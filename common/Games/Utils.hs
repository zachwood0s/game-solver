{-# LANGUAGE FlexibleContexts #-}

module Games.Utils where

import qualified Data.Vector as V
import Data.Vector ((!))
import Data.Bits
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Data.Maybe
import Data.List
import System.Random

import Games.Types
import qualified Solvers.Types as S
import Utils


{-----------------------
    Sequence Helpers
------------------------}

-- | Takes a sequence and chops it into pieces of length n
--
-- >>> chop 2 [1, 2, 3, 4]
-- [[1, 2], [2, 3], [3, 4]]
chop :: Int -> [a] -> [[a]]
chop k xs 
  | length chop' < k = []
  | otherwise = chop' : chop k (tail xs)
  where chop' = take k xs

-- | Returns the diagonal of a two dimensional list
diagonal :: [[a]] -> [a]
diagonal [] = []
diagonal ([]:_) = []
diagonal ((x:_):rows) = x : diagonal (map tail rows)

-- | Returns all the diagonal sequences in a two dimensional list
diagonals :: [[a]] -> [[a]]
diagonals m = map diagonal (init . tails $ m)
     ++ tail (map diagonal (init . tails $ transpose m))
            
-- | Returns all sequences (including diagonals) of length n from a 2-d list
getSequences :: [[a]] -> Int -> [[a]]
getSequences b len =
  rows ++ cols ++ fdiag ++ bdiag
  where 
    rows = concatMap (chop len) b
    cols = concatMap (chop len) $ transpose b
    fdiag = concatMap (chop len) $ diagonals b
    bdiag = concatMap (chop len) $ diagonals (map reverse b)   

{-----------------------
     Hashing Helpers
------------------------}
type ZobristTable = V.Vector (V.Vector (V.Vector Int))

-- | Computes the Zobrist hash 
computeZobristHash :: [[Maybe Bool]] -> ZobristTable -> Int 
computeZobristHash b zobrist = 
  foldl rowHash 0 (zip [0..] b)
  where 
    piece = fromEnum
    rowHash h (idx, row) = foldl (doHash idx) h (zip [0..] row)
    doHash _ acc (_, Nothing) = acc
    doHash row acc (col, Just x) =
      acc `xor` (zobrist ! row ! col ! piece x)

zobristTable :: Int -> Int -> Int -> ZobristTable
zobristTable seed rows cols = 
  evalState (zobristTableM rows cols) (mkStdGen 33)

zobristTableM :: Int -> Int -> State StdGen ZobristTable
zobristTableM rows cols = 
  V.replicateM rows 
  $ V.replicateM cols 
  $ V.replicateM 2 (state random)

{-----------------------
     Turn Helpers
------------------------}
type Player = Bool

isAiTurn :: (MonadReader PlayerConfig m) => Player -> m (Maybe S.Options)
isAiTurn playerTurn = 
  asks $ liftM2 firstJust p1Ai p2Ai
  where 
    isValid f player = (&& f playerTurn) . isJust . player
    p1Ai = liftM2 if' (isValid id player1) player1
    p2Ai = liftM2 if' (isValid not player2) player2

isPlayerTurn :: (MonadReader PlayerConfig m) => Player -> m Bool
isPlayerTurn playerTurn = asks (p1Valid `fOr` p2Valid) 
  where 
    p1Valid = (&& playerTurn) . isNothing . player1       -- No p1 AI and X's turn
    p2Valid = (&& not playerTurn) . isNothing . player2 -- No p2 AI and O's tur