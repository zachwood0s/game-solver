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

import qualified Solvers.Model
import qualified Solvers
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
--  | playerTurn && isJust s1 = 
update (Just s) _ DoAi m@Model{playerTurn=True, ..} = 
  let
    solverStrat = fromJust $ Data.Map.lookup (Solvers.Model.getSelectedSolver s) Solvers.solverMap
    tree = buildTree (Solvers.Model.searchDepth s) m
    moveLevels = levels $ solverStrat solver True tree
    allMoves = getAllMoves moveLevels
    (x, y) = fst $ getOptimalMove True allMoves
  in
    move x y m <# pure DoAi
  where
    getAllMoves (_ : xs : _) = xs
    getAllMoves _ = []
    getOptimalMove True nodes = maximumBy (comparing snd) (reverse nodes)
    getOptimalMove False nodes = minimumBy (comparing snd) nodes 

-- Player 2 is an AI and it is player 1's turn
update _ (Just s) DoAi m@Model{playerTurn=False, ..} = 
  noEff m

-- No Ai players given
update Nothing Nothing DoAi m = noEff m
update _ _ _ m = noEff m

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

moves :: Model -> [DecisionNode]
moves Model{gameState = Won _} = []
moves Model{gameState = Stalemate} = []
moves game@Model{boardSize = (rows, cols)} = 
  mapMaybe shouldInclude [getMove x y | x <- [0..cols-1], y <- [0..rows-1]]
  where 
    makeMove x y m = ((x, y), m)
    getMove x y = makeMove x y (move x y game)
    shouldInclude ((x, y), m) = fmap (makeMove x y) m

buildTree :: Int -> Model -> Tree DecisionNode
buildTree depth g = buildTree' depth ((-1, -1), g)
  where 
    buildTree' :: Int -> DecisionNode -> Tree DecisionNode
    buildTree' depth' node@(_, game) = Node node (getLeaves depth' game)
    getLeaves 0 _ = []
    getLeaves depth' game = map (buildTree' (depth' - 1)) (moves game)
