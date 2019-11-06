{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Games.TicTacToe where

import Debug.Trace (trace)
import Data.List
import Data.Maybe
import Control.Lens ((^.), makeLenses, set, ix)
import Control.Monad 
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State.Lazy
import qualified Data.Vector as V
import qualified Data.Map as M
import System.Random
import Miso
import Miso.String (toMisoString)
import Data.Vector ((!))
import Data.Bits

import Utils
import qualified Solvers.Types as S
import Games.Types
import Solvers as S

{----------------
    Game Model
----------------}

type Player = Bool
data GameState = Won Player | Stalemate | Running deriving (Show, Eq)
type BoardMark = Maybe Player
type BoardSize = (Int, Int)
type InputPosition = (Int, Int)
type DecisionNode = (InputPosition, Model)
type ZobristTable = V.Vector (V.Vector (V.Vector Int))

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
  , _mZobrist = evalState (zobristTable size) (mkStdGen 33)
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
    [ class_ "gameState" 
    ]
    [ text . toMisoString $ showGameState (m ^. mGameState) (m ^. mPlayerTurn)
    ]

drawBoard :: Model -> Interface action -> View action
drawBoard m iface = 
  div_ 
    [ id_ "board" ]
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
        [ span_ [] [text t]
        ]

{----------------
    Game Logic
----------------}
ticTacToe :: Model -> Game action
ticTacToe m = trace "Makin game" $ Game 
  { Games.Types.update = trace "touchin this" $ Games.TicTacToe.update m
  , Games.Types.view = Games.TicTacToe.view m
  , Games.Types.gameType = TicTacToeGame
  }

update :: Model -> Interface action -> Msg -> GameM action (Game action)
update m iface (TicTacToe msg) = do
  playerTurn <- isPlayerTurn m
  aiTurn <- isAiTurn m
  case  msg of 
    Move row col | playerTurn -> do
      gameAction iface $ TicTacToe DoAi
      gameLog iface $ print "Player Move"
      --(lift . tell) [print "Plar move" >> pure (passAction iface $ TicTacToe NoOp)]
      --(lift . tell) [pure (passAction iface $ TicTacToe DoAi)]
      return $ ticTacToe (fromMaybe m (move col row m))
    DoAi | Just s <- aiTurn -> do
      gameAction iface $ TicTacToe DoAi
      return $ ticTacToe (doAIMove s m)
    _ -> return $ ticTacToe m
--return $ 
--Wrong msg type was sent somehow
update m _ _ = return $ ticTacToe m 

isAiTurn :: (MonadReader PlayerConfig m) => Model -> m (Maybe S.Options)
isAiTurn m = 
  asks $ liftM2 firstJust p1Ai p2Ai
  where 
    isValid f player = (&& f (m ^. mPlayerTurn)) . isJust . player
    p1Ai = liftM2 if' (isValid id player1) player1
    p2Ai = liftM2 if' (isValid not player2) player2

isPlayerTurn :: (MonadReader PlayerConfig m) => Model -> m Bool
isPlayerTurn m = asks (p1Valid `fOr` p2Valid) 
  where 
    p1Valid = (&& m ^. mPlayerTurn) . isNothing . player1       -- No p1 AI and X's turn
    p2Valid = (&& not (m ^. mPlayerTurn)) . isNothing . player2 -- No p2 AI and O's turn


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
  , generateHash = computeHash
  }

computeHash :: DecisionNode -> Int 
computeHash (_, m) = 
  foldl rowHash 0 (zip [0..] (m ^. mBoard))
  where 
    piece = fromEnum
    rowHash h (idx, row) = foldl (doHash idx) h (zip [0..] row)
    doHash _ acc (_, Nothing) = acc
    doHash row acc (col, Just x) =
      acc `xor` ((m ^. mZobrist) ! row ! col ! piece x)

  
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

{----------------
    Utils
----------------}
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
