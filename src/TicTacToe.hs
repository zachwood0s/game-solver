module TicTacToe (
  newGame, startGame, evaluateSeq, Player(..), BoardMark, buildTree, board, showBoard, minimax, minimaxAB
) where

import Control.Lens (set, ix)
import Data.Maybe (isNothing, mapMaybe, catMaybes)
import Data.Either.Combinators
import Data.Tree
import Data.Ord
import Utils
import Data.List (tails, transpose, maximumBy, minimumBy)
import System.Exit
import Text.ParserCombinators.Parsec
import Solver (Solver(..), minimaxAB, minimax, ABScore(..))

data Player = O | X deriving (Show, Eq)
data GameState = Won Player | Stalemate | Running deriving (Show, Eq)
data PlayerInput = Exit | M | List | Move InputPosition deriving (Show, Eq)

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

invalidPosition :: (Int, Int)
invalidPosition = (-1, -1)

{------------------
      Helpers
------------------}

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
            
getSequences :: Board -> Int -> [[BoardMark]]
getSequences (Board b) winLen =
  rows ++ cols ++ fdiag ++ bdiag
  where 
    rows = concatMap (chop winLen) b
    cols = concatMap (chop winLen) $ transpose b
    fdiag = concatMap (chop winLen) $ diagonals b
    bdiag = concatMap (chop winLen) $ diagonals (map reverse b)

{------------------
  State Handling
------------------}

emptyBoard :: BoardSize -> Board 
emptyBoard (rows, cols) = Board $ replicate rows $ replicate cols Nothing

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X 

getGameState :: Board -> Int -> GameState
getGameState b@(Board markings) winLen 
  | isWin X = Won X
  | isWin O = Won O
  | isStalemate = Stalemate
  | otherwise = Running
  where 
    winningSeq player = all (== Just player)
    isWin player = any (winningSeq player) $ getSequences b winLen 
    isStalemate = all (notElem Nothing) markings

move :: Int -> Int -> Game -> Maybe Game
move _ _ (Game _ _ _ _ Stalemate) = Nothing
move _ _ (Game _ _ _ _ (Won _)) = Nothing
move x y (Game player (Board b) size@(rows, cols) winLen Running) 
  | validMove x y = 
      let newBoard = Board $ set (ix y . ix x) (Just player) b 
      in
        Just Game 
          { playerTurn = nextPlayer player
          , boardSize = size
          , gameState =  getGameState newBoard winLen
          , board = newBoard
          , winningSeqLen = winLen
          }
  | otherwise = Nothing 
  where 
    onBoard x' y' = inrange (0, rows - 1) y' && inrange (0, cols - 1) x'
    isEmpty x' y' = isNothing (b !! y' !! x')
    validMove = onBoard `fAnd2` isEmpty


moves :: Game -> [DecisionNode]
moves (Game _ _ _ _ (Won _)) = []
moves (Game _ _ _ _ Stalemate) = []
moves game@(Game _ _ (rows, cols) _ _) = 
  mapMaybe shouldInclude [getMove x y | x <- [0..cols-1], y <- [0..rows-1]]
  where 
    makeMove x y m = ((x, y), m)
    getMove x y = makeMove x y (move x y game)
    shouldInclude ((x, y), m) = fmap (makeMove x y) m

newGame :: BoardSize -> Int -> Game
newGame size winLen = 
  Game 
  { playerTurn = X
  , boardSize = size
  , gameState = Running
  , board = emptyBoard size
  , winningSeqLen = winLen
  } 

{------------------
  Input Handling
------------------}

startGame :: Game -> IO ()
startGame game = do 
  putStrLn $ "Player " ++ show (playerTurn game) ++ "'s turn"
  line <- getLine
  modifiedGame <- takeInput game (readInput line)
  maybe retry next modifiedGame
  where
    takeInput game' = maybe (return Nothing) (handleInput game')
    retry = putStrLn "Invalid move. Please input a valid move." >> startGame game
    next newGame' = do
      putStrLn $ showBoard (board newGame')
      case gameState newGame' of
        Won a      -> putStrLn $ "Player " ++ show a ++ " won!"
        Stalemate  -> putStrLn "It's a draw!"
        Running    -> startGame newGame'

handleInput :: Game -> PlayerInput -> IO (Maybe Game)
handleInput _ Exit = exitSuccess
handleInput game (Move (i, j)) = return $ move i j game
handleInput game M = 
  let 
    moveLevels = levels $ minimaxAB ticTacToeSolver (isMaximizing $ playerTurn game) (buildTree (5) game)
    allMoves = getAllMoves moveLevels
    (x, y) = fst $ getOptimalMove (playerTurn game) allMoves
  in
    return $ move x y game
  where
    getAllMoves (_ : xs : _) = xs
    getAllMoves _ = []
    getOptimalMove X nodes = maximumBy (comparing snd) (reverse nodes)
    getOptimalMove O nodes = minimumBy (comparing snd) nodes

handleInput game List = do
  --putStrLn $ drawTree $ fmap show node
  putStrLn $ unlines $ map toString options
  return $ Just game
  where 
    (Node _ options) = 
      minimaxAB ticTacToeSolver (isMaximizing $ playerTurn game) (buildTree (5) game)
    toString (Node (p, s) _) = show p ++ " - " ++ show s

isMaximizing :: Player -> Bool
isMaximizing X = True
isMaximizing O = False

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
      playerMove <- between (symbol '(') (symbol ')') $ do
        x <- read <$> integer
        _ <- symbol ','
        y <- read <$> integer
        return (Move (x, y))
      eof
      return playerMove

    integer = lexeme (many1 digit)
    parseToken text val= do 
      _ <- lexeme (string text)
      eof 
      return val

-- Solvers

evalScore :: Game -> Int
evalScore (Game _ b _ winLen _) = 
  sum $ map evaluateSeq (getSequences b winLen)

-- Evaluation Huristic
-- 1, 10, 100 for every one in a row of the same piece
evaluateSeq :: [BoardMark] -> Int
evaluateSeq s = 
  let 
    allMarks = catMaybes s -- Get rid of empty squares
    cnt = length allMarks 
  in 
    if all (== X) allMarks then 10^cnt
    else if all(== O) allMarks then -(10^cnt)
    else 0

buildTree :: Int -> Game -> Tree DecisionNode
buildTree depth g = buildTree' depth (invalidPosition, g)
  where 
    buildTree' :: Int -> DecisionNode -> Tree DecisionNode
    buildTree' depth' node@(_, game) = Node node (getLeaves depth' game)
    getLeaves 0 _ = []
    getLeaves depth' game = map (buildTree' (depth' - 1)) (moves game)


ticTacToeSolver :: Solver DecisionNode (InputPosition, ABScore Int)
ticTacToeSolver = Solver
  { getScore = snd
  , evaluateScore = \(_, g) -> evalScore g
  , buildNode = \s (p, _) -> (p, s)
  }