module TicTacToe.Model 
  ( TicTacToeOptions(..), showBoard
  , Model(..), BoardSize
  , emptyModel
  ) where

type Player = Bool
data GameState = Won Player | Stalemate | Running deriving (Show, Eq)
type BoardMark = Maybe Player
type BoardSize = (Int, Int)
--type InputPosition = (Int, Int)
--type DecisionNode = (InputPosition, Game)

type Board = [[BoardMark]] 
showBoard :: Board -> String
showBoard grid = unlines ( map showRow grid)
    where 
      showRow = unwords . map showMark
      showMark Nothing = "_"
      showMark (Just a) = show a 

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
