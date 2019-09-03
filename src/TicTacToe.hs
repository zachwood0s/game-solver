module TicTacToe (
  newGame, startGame, evaluateSeq, Player(..), BoardMark, buildTree, board, showBoard, minimax
) where

import Control.Lens (set, ix)
import Data.Maybe (isNothing, mapMaybe, fromMaybe, catMaybes)
import Data.Either 
import Data.Either.Combinators
import Data.Tree
import Utils
import Data.Function
import Text.Read (readMaybe)
import Control.Monad
import Data.List (tails, transpose)
import System.Exit
import Text.ParserCombinators.Parsec

data Player = O | X deriving (Show, Eq)
data GameState = Won Player | Stalemate | Running deriving (Show, Eq)
type BoardMark = Maybe Player
type BoardSize = (Int, Int)

type InputPosition = (Int, Int)
data PlayerInput = Exit | M | List | Move InputPosition deriving (Show, Eq)

readInput :: String -> Maybe PlayerInput
readInput input = rightToMaybe $ parse inputParser "" input

inputParser :: Parser PlayerInput
inputParser = 
  parseExit 
  <|> parseM 
  <|> parseList 
  <|> parseMove
  where 
    parseExit = parseToken "Exit" Exit
    parseM = parseToken "M" M
    parseList = parseToken "List" List
    parseMove = do 
      move <- between (symbol '(') (symbol ')') $ do
        x <- read <$> integer
        symbol ','
        y <- read <$> integer
        return (Move (x, y))
      eof
      return move

    integer = lexeme (many1 digit)
    parseToken text val= do 
      lexeme (string text)
      eof 
      return val


newtype Board = Board [[BoardMark]] deriving (Eq, Show)
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

type DecisionNode = (InputPosition, Game)

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


moves :: Game -> [DecisionNode]
moves game@(Game _ _ (rows, cols) _ _) = 
  mapMaybe shouldInclude [getMove x y | x <- [0..rows-1], y <- [0..cols-1]]
  where 
    makeMove x y m = ((x, y), m)
    getMove x y = makeMove x y (move x y game)
    shouldInclude ((x, y), m) = fmap (makeMove x y) m

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
  putStrLn $ "Player " ++ show (playerTurn game) ++ "'s turn"
  line <- getLine
  newGame <- takeInput game (readInput line)
  maybe retry next newGame
  where
    takeInput game = maybe (return Nothing) (handleInput game)
    retry = putStrLn "Invalid move. Please input a valid move." >> startGame game
    next newGame = do
      putStrLn $ showBoard (board newGame)
      case gameState newGame of
        Won a      -> putStrLn $ "Player " ++ show a ++ " won!"
        Stalemate  -> putStrLn "It's a draw!"
        Running    -> startGame newGame

handleInput :: Game -> PlayerInput -> IO (Maybe Game)
handleInput game Exit = exitSuccess
handleInput game (Move (i, j)) = return $ move j i game
handleInput game M = do
  print $ minimax (playerTurn game) (buildTree (-1) game) 
  return $ Just game

handleInput game List = do
  putStrLn $ unlines $ map (\(pos, g) -> show pos) (moves game)
  return $ Just game


-- Solvers

evaluateScore :: Game -> Int
evaluateScore (Game _ board _ winLen _) = 
  sum $ map evaluateSeq (getSequences board winLen)


-- Evaluation Huristic
-- 1, 10, 100 for every one in a row of the same piece
evaluateSeq :: [BoardMark] -> Int
evaluateSeq seq = 
  let 
    allMarks = catMaybes seq -- Get rid of empty squares
    count = length allMarks 
  in 
    if all (== X) allMarks then 10^count
    else if all (== O) allMarks then -(10^count)
    else 0


buildTree :: Int -> Game -> Tree DecisionNode
buildTree depth g = buildTree' depth ((-1, -1), g)
  where 
    buildTree' :: Int -> DecisionNode -> Tree DecisionNode
    buildTree' depth node@(_, game) =
      let 
        leaves = 
          if depth /= 0 then 
            map (buildTree' (depth - 1)) (moves game)
          else 
            [] 
      in 
        Node node leaves


minimax :: Player -> Tree DecisionNode -> Int 
minimax _ (Node (_, g) []) = evaluateScore g
minimax X (Node _ ts) = maximum (map (minimax O) ts)
minimax O (Node _ ts) = minimum (map (minimax X) ts)

