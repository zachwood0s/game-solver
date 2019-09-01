module TicTacToe (
  newGame, startGame
) where

import Control.Lens (set, ix)
import Data.Maybe (isNothing, catMaybes)
import Utils
import Data.Function
import Text.Read (readMaybe)
import Data.List (tails, transpose)

data Player = O | X deriving (Show, Eq)
data GameState = Won Player | Stalemate | Running deriving (Show, Eq)
type BoardMark = Maybe Player
type BoardSize = (Int, Int)

newtype Board = Board [[BoardMark]] deriving (Eq)

instance Show Board where
  show (Board grid) = unlines ( map showRow grid)
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
  }

emptyBoard :: BoardSize -> Board 
emptyBoard (rows, cols) = Board $ replicate rows $ replicate cols Nothing

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X 

chop :: Int -> [a] -> [[a]]
chop k xs 
  | length chop' < k = []
  | otherwise = chop' : chop k (tail xs)
  where chop' = take k xs

diagonal :: [[a]] -> [a]
diagonal [] = []
diagonal ((x:_):rows) = x : diagonal (map tail rows)

diagonals :: [[a]] -> [[a]]
diagonals m = map diagonal (init . tails $ m)
     ++ tail (map diagonal (init . tails $ transpose m))
            
getSequences :: Board -> Int -> [[BoardMark]]
getSequences (Board board) winLen =
  rows ++ cols ++ fdiag ++ bdiag
  where 
    rows = concatMap (chop winLen) board
    cols = concatMap (chop winLen) $ transpose board
    fdiag = concatMap (chop winLen) $ diagonals board
    bdiag = concatMap (chop winLen) $ diagonals (map reverse board)

getGameState :: Board -> Int -> GameState
getGameState board@(Board markings) winLen 
  | isWin X = Won X
  | isWin O = Won O
  | isStalemate = Stalemate
  | otherwise = Running
  where 
    winningSeq player = all (== Just player)
    isWin player = any (winningSeq player) $ getSequences board winLen 
    isStalemate = all (notElem Nothing) markings

move :: Int -> Int -> Game -> Maybe Game
move x y (Game _ _ _ _ Stalemate) = Nothing
move x y (Game _ _ _ _ (Won _)) = Nothing
move x y (Game player (Board board) boardSize@(rows, cols) winLen Running) 
  | validMove x y = 
      let newBoard = Board $ set (ix x . ix y) (Just player) board 
      in
        Just Game 
          { playerTurn = nextPlayer player
          , boardSize = boardSize
          , gameState =  getGameState newBoard winLen
          , board = newBoard
          , winningSeqLen = winLen
          }
  | otherwise = Nothing 
  where 
    onBoard x y = inrange (0, rows - 1) x && inrange (0, cols - 1) y
    isEmpty x y = isNothing (board !! x !! y)
    validMove = onBoard `fAnd2` isEmpty


moves :: Game -> [Game]
moves game@(Game _ _ (rows, cols) _ _) = 
  catMaybes [move x y game | x <- [0..rows-1], y <- [0..cols-1]]

newGame :: BoardSize -> Int -> Game
newGame boardSize winLen = 
  Game 
  { playerTurn = X
  , boardSize = boardSize
  , gameState = Running
  , board = emptyBoard boardSize
  , winningSeqLen = winLen
  }

startGame :: Game -> IO ()
startGame game = do 
  line <- getLine
  maybe retry next $ do
    (i, j) <- readMaybe line
    move i j game
  where
    retry = putStrLn "Invalid move. Please input a valid move." >> startGame game
    next newGame = do
      print (board newGame)
      case gameState newGame of
        Won a      -> putStrLn $ "Player " ++ show a ++ " won!"
        Stalemate  -> putStrLn "It's a draw!"
        Running    -> startGame newGame