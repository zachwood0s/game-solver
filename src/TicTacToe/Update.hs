{-# LANGUAGE RecordWildCards #-}
module TicTacToe.Update
  ( TicTacToe.Update.update
  ) where

import Miso
import Control.Lens (set, ix)
import Data.Map (lookup)
import Data.Maybe
import Data.List (tails, transpose, maximumBy, minimumBy)
import Data.Ord
import Data.Tree

import GHCJS.Types
import GHCJS.Foreign.Callback
import GHCJS.Marshal

import qualified Solvers.Model
import Solvers (runSolverFromString)
import TicTacToe.Messages 
import TicTacToe.Model
import Utils
import Debug.Trace

update :: Maybe Solvers.Model.Options -> Maybe Solvers.Model.Options -> Msg -> Model -> Effect Msg Model
-- Player 1 is not an AI and it is player 1's turn
update Nothing _ (Move row col) m@Model{playerTurn=True} = 
  fromMaybe m (move col row m) <# pure DoAi

-- Player 2 is not an AI and it is player 2's turn
update _ Nothing (Move row col) m@Model{playerTurn=False} = 
  fromMaybe m (move col row m) <# pure DoAi

-- Player 1 is an AI and it is player 1's turn
--update s1 s2 DoAi m@Model{..} 
--playerTurn && isJust s1 = 

update (Just s) _ DoAi m@Model{playerTurn = True, gameState=Running} = doAIMove s m 
update _ (Just s) DoAi m@Model{playerTurn = False, gameState=Running} = doAIMove s m 
update Nothing Nothing DoAi m = noEff m

update _ _ _ m = noEff m

doAIMove :: Solvers.Model.Options -> Model -> Effect Msg Model 
doAIMove s@Solvers.Model.Options{..} m@Model{..} = 
  let 
    key = Solvers.Model.getSelectedSolver s
    optimalMove = snd $ runSolverFromString key solver playerTurn searchDepth ((-1, -1), m)
  in 
    trace ("applying: " ++ show optimalMove) $ fromMaybe m (uncurry move optimalMove m) <# pure DoAi

move :: Int -> Int -> Model -> Maybe Model 
move _ _ Model{gameState = Stalemate} = Nothing
move _ _ Model{gameState = Won _} = Nothing
move x y Model{..} 
  | validMove x y = 
      let newBoard = Control.Lens.set (ix y . ix x) (Just playerTurn) board
      in
        Just Model 
          { playerTurn = nextPlayer playerTurn
          , boardSize = boardSize
          , gameState =  getGameState newBoard winningSeqLen
          , board = newBoard
          , winningSeqLen = winningSeqLen
          }
  | otherwise = Nothing 
  where 
    (rows, cols) = boardSize
    onBoard x' y' = inrange (0, rows - 1) y' && inrange (0, cols - 1) x'
    isEmpty x' y' = isNothing (board !! y' !! x')
    validMove = onBoard `fAnd2` isEmpty

nextPlayer :: Player -> Player
nextPlayer = not

getGameState :: Board -> Int -> GameState
getGameState b winLen 
  | isWin True = Won True
  | isWin False = Won False
  | isStalemate = Stalemate
  | otherwise = Running
  where 
    winningSeq player = all (== Just player)
    isWin player = any (winningSeq player) $ getSequences b winLen 
    isStalemate = all (notElem Nothing) b

moves :: DecisionNode -> [DecisionNode]
moves (_, Model{gameState = Won _}) = []
moves (_, Model{gameState = Stalemate}) = []
moves (_, game@Model{boardSize = (rows, cols)}) = 
  mapMaybe shouldInclude [getMove x y | x <- [0..cols-1], y <- [0..rows-1]]
  where 
    makeMove x y m = ((x, y), m)
    getMove x y = makeMove x y (move x y game)
    shouldInclude ((x, y), m) = fmap (makeMove x y) m

{-
buildTree :: Int -> Model -> Tree DecisionNode
buildTree depth g = buildTree' depth ((-1, -1), g)
  where 
    buildTree' :: Int -> DecisionNode -> Tree DecisionNode
    buildTree' depth' node@(_, game) = Node node (getLeaves depth' game)
    getLeaves 0 _ = []
    getLeaves depth' game = map (buildTree' (depth' - 1)) (moves game)
-}

solver :: Solvers.Model.Solver DecisionNode InputPosition
solver = Solvers.Model.Solver 
  { evaluateScore = evalScore . snd
  , buildNode = \s (p, _) -> (s, p)
  , getMoves = moves
  , nextPlayer = \_ current -> not current
  , showNode = show
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

foreign import javascript unsafe "document.write($1);"
  documentWrite :: JSVal -> IO ()