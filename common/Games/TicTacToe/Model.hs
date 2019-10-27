module Games.TicTacToe.Model 
  ( TicTacToeOptions(..) 
  , Model(..), BoardSize, GameState(..), Board, Player, DecisionNode, BoardMark, InputPosition
  , emptyModel, showPlayer, showGameState
  ) where

import Data.List
import Data.Maybe
import Control.Monad 
import Control.Monad.State.Lazy
import qualified Data.Vector as V
import System.Random

import Solvers.Types

type Player = Bool
data GameState = Won Player | Stalemate | Running deriving (Show, Eq)
type BoardMark = Maybe Player
type BoardSize = (Int, Int)
type InputPosition = (Int, Int)
type DecisionNode = (InputPosition, Model)
type ZobristTable = V.Vector (V.Vector (V.Vector Int))

type Board = [[BoardMark]] 

data Model = Model 
  { playerTurn :: Player
  , board :: Board
  , boardSize :: BoardSize
  , winningSeqLen :: Int
  , gameState :: GameState
  , zobrist :: ZobristTable
  } deriving (Show, Eq)

data TicTacToeOptions = TicTacToeOptions
  { selectedBoardSize :: BoardSize 
  , selectedWinningSeqLen :: Int
  } deriving Eq


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
  , zobrist = evalState (zobristTable size) (mkStdGen 33)
  } 

showPlayer :: Player -> String
showPlayer True = "X"
showPlayer False = "O"

showGameState :: GameState -> Player -> String 
showGameState (Won x) _= showPlayer x ++ " Won"
showGameState Stalemate _= "Draw"
showGameState _ x = showPlayer x ++ "'s Turn"

zobristTable :: BoardSize -> State StdGen ZobristTable
zobristTable (rows, cols) = 
  V.replicateM rows 
  $ V.replicateM cols 
  $ V.replicateM 2 (state random)