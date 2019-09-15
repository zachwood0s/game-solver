module TicTacToe.Model 
  ( TicTacToeOptions(..), showBoard
  ) where

import Solvers.Types

data Player = O | X deriving (Show, Eq)
data GameState = Won Player | Stalemate | Running deriving (Show, Eq)
type BoardMark = Maybe Player
type BoardSize = (Int, Int)
type InputPosition = (Int, Int)
type DecisionNode = (InputPosition, Game)

newtype Board = Board [[BoardMark]] deriving (Eq, Show)
showBoard :: Board -> String
showBoard (Board grid) = unlines ( map showRow grid)
    where 
      showRow = unwords . map showMark
      showMark Nothing = "_"
      showMark (Just a) = show a 

data Game = Game 
  { playerTurn :: Player
  , board :: Board
  , boardSize :: BoardSize
  , winningSeqLen :: Int
  , gameState :: GameState
  } deriving Show

data TicTacToeOptions = TicTacToeOptions
  { selectedBoardSize :: BoardSize 
  , selectedWinningSeqLen :: Int
  , solverOptions :: SolverOptions DecisionNode (InputPosition, ABScore Int)
  }