{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Games.Connect4 where

import Debug.Trace
import Control.Lens ((^.), makeLenses, set, ix)
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Miso 
import Miso.String (toMisoString)

import Utils
import qualified Solvers as S
import qualified Solvers.Types as S 
import Games.Types
import Games.Utils
import Games.Messages.Connect4

{----------------
    Game Model
----------------}

data GameState = Won Player | Stalemate | Running deriving (Show, Eq)
type BoardMark = Maybe Player 
type BoardSize = (Int, Int)
type InputPosition = Int 
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

emptyBoard :: BoardSize -> Board 
emptyBoard (rows, cols) = replicate rows $ replicate cols Nothing 

emptyModel :: BoardSize -> Int -> Model 
emptyModel size winLen = Model 
  { _mPlayerTurn = True 
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
    [ id_ "board", class_ "connect4"]
    (drawDroppers m iface : mapInd drawRow (m ^. mBoard))
  where
    (rows, cols) = m ^. mBoardSize
    width = 100 / fromIntegral cols
    drawRow row ind = div_ [ class_ "row" ] $ mapInd (drawCol ind) row
    drawCol rowInd (Just player) colInd = drawCell rowInd colInd player
    drawCol rowInd Nothing colInd = 
      div_ [class_ "col", colStyle] []
    drawCell row col player = 
      div_ 
        [ classList_ 
          [ ("col", True)
          , ("redPlayer", player)
          , ("yellowPlayer", not player)
          ]
        , colStyle
        ]
        []
    colStyle = style_ $ M.singleton "width" $ toMisoString ("calc("++ show width ++ "% - var(--game-connect4-margin) * 2)") 

drawDroppers :: Model -> Interface action -> View action 
drawDroppers m iface =
  div_ 
    [ id_ "top"
    , classList_ 
      [ ("redPlayer", m ^. mPlayerTurn)
      , ("yellowPlayer", not (m ^. mPlayerTurn))
      , ("row", True)
      ]
    ]
    $ map drawDropper [0..cols-1]
  where 
    cols = snd (m ^. mBoardSize)
    width = 100 / fromIntegral cols
    colStyle = style_ $ M.singleton "width" $ toMisoString ("calc("++ show width ++ "% - var(--game-connect4-margin) * 2)") 
    drawDropper col = 
      div_ 
        [ class_ "col"
        , onClick (passAction iface $ Connect4 $ Move col)
        , colStyle
        ]
        []
    classes p =
      classList_ 
        [ ("redPlayer", p)
        , ("yellowPlayer", not p)
        , ("row", True)
        ]

showPlayer :: Player -> String
showPlayer True = "Red"
showPlayer False = "Yellow"

showGameState :: GameState -> Player -> String 
showGameState (Won x) _= showPlayer x ++ " Won"
showGameState Stalemate _= "Draw"
showGameState _ x = showPlayer x ++ "'s Turn"

{----------------
    Game Logic
----------------}
connect4 :: Model -> Game action
connect4 m = Game 
  { Games.Types.update = Games.Connect4.update m 
  , Games.Types.view = Games.Connect4.view m 
  , Games.Types.gameType = Connect4Game
  }

update :: Model -> Interface action -> Games.Types.Msg -> GameM action (Game action)
update m iface (Connect4 msg) = do 
  playerTurn <- isPlayerTurn (m ^. mPlayerTurn)
  aiTurn <- isAiTurn (m ^. mPlayerTurn)
  case msg of 
    Move col | playerTurn -> do 
      gameAction iface $ Connect4 DoAi
      return $ connect4 (fromMaybe m (move col m))
    DoAi | Just s <- aiTurn -> do 
      gameAction iface $ Connect4 DoAi 
      return $ connect4 (doAIMove s m)
    _ -> return $ connect4 m

-- Wrong msg type was sent 
update m _ _ = return $ connect4 m

doAIMove :: S.Options -> Model -> Model 
doAIMove s m = 
  let 
    key = S.getSelectedSolver s
    depth = s ^. S.mSearchDepth
    turn = m ^. mPlayerTurn
    solved = S.runSolverFromString key solver turn depth (-1, m)
    optimalMove = snd solved
  in 
    fromMaybe m (move optimalMove m) 

moves :: Model -> [DecisionNode]
moves Model{_mGameState = Won _} = []
moves Model{_mGameState = Stalemate} = []
moves m = 
  mapMaybe (shouldInclude . getMove) [0..cols-1]
  where 
    (rows, cols) = m ^. mBoardSize
    getMove x = (x, move x m)
    shouldInclude (x, m) = fmap (x,) m

move :: Int -> Model -> Maybe Model 
move _ Model{_mGameState = Stalemate} = Nothing 
move _ Model{_mGameState = Won _} = Nothing 
move x m 
  | validMove x = 
    let 
      y = lastEmpty (transpose (m ^. mBoard) !! x) 0
      newBoard = trace ("y pos" ++ show y) Control.Lens.set (ix y . ix x) (Just (m ^. mPlayerTurn)) (m ^. mBoard)
    in 
      Just m 
        { _mPlayerTurn = getNextPlayer (m ^. mPlayerTurn)
        , _mGameState = getGameState newBoard (m ^. mWinningSeqLen)
        , _mBoard = newBoard
        }
  | otherwise = Nothing
  where 
    cols = snd (m ^. mBoardSize)
    onBoard = inrange (0, cols -1)
    hasSlots = isNothing . (head (m ^. mBoard) !!)
    validMove = onBoard `fAnd` hasSlots

lastEmpty :: [BoardMark] -> Int -> Int
lastEmpty [] cnt = cnt - 1
lastEmpty (x:xs) cnt = 
  if isJust x then cnt - 1
  else lastEmpty xs (cnt + 1)

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
  { S.evaluateScore = evalScore . snd
  , S.buildNode = \s (p, _) -> (s, p)
  , S.getMoves = moves . snd
  , S.nextPlayer = const getNextPlayer
  , S.showNode = show
  , S.generateHash = \(_, m) -> computeZobristHash (m ^. mBoard) (m ^. mZobrist)
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