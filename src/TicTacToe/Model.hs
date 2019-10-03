{-# LANGUAGE RecordWildCards #-}

module TicTacToe.Model 
  ( TicTacToeOptions(..) 
  , Model(..), BoardSize, GameState(..), Board, Player, DecisionNode
  , emptyModel, showPlayer, showGameState, solver, getSequences
  ) where

import Data.List
import Data.Maybe
import Solvers.Model

type Player = Bool
data GameState = Won Player | Stalemate | Running deriving (Show, Eq)
type BoardMark = Maybe Player
type BoardSize = (Int, Int)
type InputPosition = (Int, Int)
type DecisionNode = (InputPosition, Model)

type Board = [[BoardMark]] 

data Model = Model 
  { playerTurn :: Player
  , board :: Board
  , boardSize :: BoardSize
  , winningSeqLen :: Int
  , gameState :: GameState
  } deriving (Show, Eq)

data TicTacToeOptions = TicTacToeOptions
  { selectedBoardSize :: BoardSize 
  , selectedWinningSeqLen :: Int
  } deriving Eq

solver :: Solver DecisionNode (InputPosition, ABScore Int)
solver = Solver 
  { getScore = snd 
  , evaluateScore = evalScore . snd
  , buildNode = \s (p, _) -> (p, s)
  }
  
evalScore :: Model -> Int
evalScore Model{..} = 
  sum $ map evaluateSeq (getSequences board winningSeqLen)

-- Evaluation Huristic
-- 1, 10, 100 for every one in a row of the same piece
evaluateSeq :: [BoardMark] -> Int
evaluateSeq s = 
  let 
    allMarks = catMaybes s -- Get rid of empty squares
    cnt = length allMarks 
  in 
    if and allMarks then 10^cnt
    else if not (or allMarks) then -(10^cnt)
    else 0

emptyBoard :: BoardSize -> Board 
emptyBoard (rows, cols) = replicate rows $ replicate cols Nothing

emptyModel :: BoardSize -> Int -> Model
emptyModel size winLen = 
  Model 
  { playerTurn = True -- X's turn
  , boardSize = size
  , gameState = Running
  , board = emptyBoard size
  , winningSeqLen = winLen
  } 

showPlayer :: Player -> String
showPlayer True = "X"
showPlayer False = "O"

showGameState :: GameState -> Player -> String 
showGameState (Won x) _= showPlayer x ++ " Won"
showGameState Stalemate _= "Draw"
showGameState _ x = showPlayer x ++ "'s Turn"

chop :: Int -> [a] -> [[a]]
chop k xs 
  | length chop' < k = []
  | otherwise = chop' : chop k (tail xs)
  where chop' = take k xs

diagonal :: [[a]] -> [a]
diagonal [] = []
diagonal ([]:_) = []
diagonal ((x:_):rows) = x : diagonal (map tail rows)

diagonals :: [[a]] -> [[a]]
diagonals m = map diagonal (init . tails $ m)
     ++ tail (map diagonal (init . tails $ transpose m))
            
getSequences :: Board -> Int -> Board
getSequences b winLen =
  rows ++ cols ++ fdiag ++ bdiag
  where 
    rows = concatMap (chop winLen) b
    cols = concatMap (chop winLen) $ transpose b
    fdiag = concatMap (chop winLen) $ diagonals b
    bdiag = concatMap (chop winLen) $ diagonals (map reverse b)