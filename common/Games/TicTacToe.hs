{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Games.TicTacToe where

import Debug.Trace (trace)
import Data.List
import Data.Maybe
import Control.Lens ((^.), makeLenses, set, ix)
import Control.Monad 
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.Map as M
import Miso
import Miso.String (toMisoString)
import Data.Bits

import Utils
import qualified Solvers.Types as S
import Games.Types
import Games.Utils
import Games.Messages.TicTacToe
import qualified Solvers as S

{----------------
    Game Model
----------------}

data GameState = Won Player | Stalemate | Running deriving (Show, Eq)
type BoardMark = Maybe Player
type BoardSize = (Int, Int)
type InputPosition = (Int, Int)
type DecisionNode = (InputPosition, Model)

type Board = [[BoardMark]] 

data Model = Model 
  { _mPlayerTurn :: Player
  , _mBoard :: Board
  , _mBoardSize :: BoardSize
  , _mWinningSeqLen :: Int
  , _mGameState :: GameState
  , _mZobrist :: ZobristTable
  } deriving (Show, Eq)

makeLenses ''Model

data TicTacToeOptions = TicTacToeOptions
  { selectedBoardSize :: BoardSize 
  , selectedWinningSeqLen :: Int
  } deriving Eq

emptyBoard :: BoardSize -> Board 
emptyBoard (rows, cols) = replicate rows $ replicate cols Nothing

emptyModel :: BoardSize -> Int -> Model
emptyModel size winLen = 
  Model 
  { _mPlayerTurn = True -- X's turn
  , _mBoardSize = size
  , _mGameState = Running
  , _mBoard = emptyBoard size
  , _mWinningSeqLen = winLen
  , _mZobrist = uncurry (zobristTable 33) size
  } 

{----------------
    View
----------------}
view :: Model -> Interface action -> View action
view m iface = 
  div_ 
    []
    [ div_ 
      [ id_ "statusBar" ]
      [ drawStatus m ] 
    , div_ 
      [ id_ "gameCanvas" ] 
      [ drawBoard m iface ]
    ]

drawStatus :: Model -> View action
drawStatus m=
  h2_ 
    [ class_ "gameState" ]
    [ text . toMisoString $ showGameState (m ^. mGameState) (m ^. mPlayerTurn) ]

drawBoard :: Model -> Interface action -> View action
drawBoard m iface = 
  div_ 
    [ id_ "board", class_ "ticTacToe"]
    $ mapInd drawRow (m ^. mBoard)
  where
    (rows, cols) = m ^. mBoardSize
    width = 100 `quot` cols
    drawRow row ind = div_ [ class_ "row" ] $ mapInd (drawCol ind) row
    drawCol rowInd Nothing colInd = drawCell rowInd colInd ""
    drawCol rowInd (Just player) colInd = drawCell rowInd colInd (toMisoString $ showPlayer player)
    drawCell row col t = 
      div_ 
        [ class_ "col"
        , style_ $ M.singleton "width" $ toMisoString (show width ++ "%")
        , onClick (passAction iface $ TicTacToe $ Move row col)
        ]
        [ span_ [] [text t] ]

showPlayer :: Player -> String
showPlayer True = "X"
showPlayer False = "O"

showGameState :: GameState -> Player -> String 
showGameState (Won x) _= showPlayer x ++ " Won"
showGameState Stalemate _= "Draw"
showGameState _ x = showPlayer x ++ "'s Turn"

{----------------
    Game Logic
----------------}
ticTacToe :: Model -> Game action
ticTacToe m = Game 
  { Games.Types.update = Games.TicTacToe.update m
  , Games.Types.view = Games.TicTacToe.view m
  , Games.Types.gameType = TicTacToeGame
  }

update :: Model -> Interface action -> Games.Types.Msg -> GameM action (Game action)
update m iface (TicTacToe msg) = do
  playerTurn <- isPlayerTurn (m ^. mPlayerTurn)
  aiTurn <- isAiTurn (m ^. mPlayerTurn)
  case  msg of 
    Move row col | playerTurn -> do
      gameAction iface $ TicTacToe DoAi
      return $ ticTacToe (fromMaybe m (move col row m))
    DoAi | Just s <- aiTurn -> do
      gameAction iface $ TicTacToe DoAi
      return $ ticTacToe (doAIMove s m)
    _ -> return $ ticTacToe m
--Wrong msg type was sent somehow
update m _ _ = return $ ticTacToe m 

doAIMove :: S.Options -> Model -> Model 
doAIMove s m = 
  let 
    key = S.getSelectedSolver s
    depth = s ^. S.mSearchDepth
    turn = m ^. mPlayerTurn
    solved = S.runSolverFromString key solver turn depth ((-1, -1), m)
    optimalMove = snd solved
  in 
    fromMaybe m (uncurry move optimalMove m) 
      
moves :: DecisionNode -> [DecisionNode]
moves (_, Model{_mGameState = Won _}) = []
moves (_, Model{_mGameState = Stalemate}) = []
moves (_, game@Model{_mBoardSize = (rows, cols)}) = 
  mapMaybe shouldInclude [getMove x y | x <- [0..cols-1], y <- [0..rows-1]]
  where 
    makeMove x y m = ((x, y), m)
    getMove x y = makeMove x y (move x y game)
    shouldInclude ((x, y), m) = fmap (makeMove x y) m

move :: Int -> Int -> Model -> Maybe Model 
move _ _ Model{_mGameState = Stalemate} = Nothing
move _ _ Model{_mGameState = Won _} = Nothing
move x y m
  | validMove x y = 
      let newBoard = Control.Lens.set (ix y . ix x) (Just (m ^. mPlayerTurn)) (m ^. mBoard)
      in 
        Just m 
          { _mPlayerTurn = getNextPlayer (m ^. mPlayerTurn)
          , _mGameState =  getGameState newBoard (m ^. mWinningSeqLen)
          , _mBoard = newBoard
          }
  | otherwise = Nothing 
  where 
    (rows, cols) = m ^. mBoardSize
    onBoard x' y' = inrange (0, rows - 1) y' && inrange (0, cols - 1) x'
    isEmpty x' y' = isNothing ((m ^. mBoard) !! y' !! x')
    validMove = onBoard `fAnd2` isEmpty

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

getNextPlayer :: Player -> Player 
getNextPlayer = not

{----------------
    Solver
----------------}
solver :: S.Solver DecisionNode InputPosition
solver = S.Solver 
  { evaluateScore = evalScore . snd
  , buildNode = \s (p, _) -> (s, p)
  , getMoves = moves
  , nextPlayer = const getNextPlayer
  , showNode = show
  , generateHash = \(_, m) -> computeZobristHash (m ^. mBoard) (m ^. mZobrist)
  }

  
evalScore :: Model -> Int
evalScore m = 
  sum $ map evaluateSeq (getSequences (m ^. mBoard) (m ^. mWinningSeqLen))

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

