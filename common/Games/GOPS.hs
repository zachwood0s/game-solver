{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Games.GOPS where

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
import Games.Messages.GOPS
import qualified Solvers as S

{----------------
    Game Model
----------------}

data GameState = Won Player | Stalemate | Running deriving (Show, Eq)

type Card = Int
type DecisionNode = (Card, Model)
type Hand = [Card]

data Model = Model 
  { _mPlayerTurn :: Player 
  , _mPlayerHands :: [Hand]
  , _mSelectedCards :: [Maybe Card]
  , _mCardAmount :: Int
  , _mGameState :: GameState
  } deriving (Show, Eq)


makeLenses ''Model

data GOPSOptions = GOPSOptions 
  { selectedCardAmt :: Int
  } deriving Eq

emptyHand :: Int -> Hand 
emptyHand cardAmt = [1..cardAmt+1]

emptyModel :: Int -> Model 
emptyModel cardAmt = 
  Model 
  { _mPlayerTurn = True -- Player 1's Turn 
  , _mPlayerHands = take 2 $ repeat (emptyHand cardAmt)
  , _mSelectedCards = take 2 $ repeat Nothing
  , _mCardAmount = cardAmt
  , _mGameState = Running
  }

{----------------
    View
----------------}
view :: Model -> Interface action -> View action
view m iface = 
  div_ 
    []
    [ div_
      [ id_ "gameCanvas" ]
      (map (flip viewCard iface) [1..m^.mCardAmount])
    ]

viewCard :: Card -> Interface action -> View action
viewCard c iface = 
  a_ 
    [ onClick (passAction iface $ GOPS $ Move c) ]
    [ text . toMisoString $ show c ]
  
{----------------
    Game Logic
----------------}
gops :: Model -> Game action
gops m = Game 
  { Games.Types.update = Games.GOPS.update m
  , Games.Types.view = Games.GOPS.view m
  , Games.Types.gameType = GOPSGame
  }

update :: Model -> Interface action -> Games.Types.Msg -> GameM action (Game action)
update m iface DoAI = return $ gops m
update m iface (GOPS msg) = do
  playerTurn <- isPlayerTurn (m ^. mPlayerTurn)
  case msg of 
    Move cardVal | playerTurn -> do 
      trace "hit" $ gameLog iface (putStrLn "SelectedCard")
      return $ gops m 
    _ -> return $ gops m
update m _ _ = return $ gops m

move :: Card -> Model -> Maybe Model
move _ Model{_mGameState = Stalemate} = Nothing
move _ Model{_mGameState = Won _} = Nothing
move c m 
  | validMove c =
      let newHands = map updateHand $ zip [1..] (m ^. mPlayerHands)
      in 
        Just m 
          { _mPlayerTurn = getNextPlayer (m ^. mPlayerTurn)
          , _mGameState = getGameState newHands 
          , _mPlayerHands = newHands
          }
  | otherwise = Nothing
  where 
    validMove = flip elem ((m ^. mPlayerHands) !! playerIx)
    playerIx = fromEnum (m ^. mPlayerTurn)
    updateHand (ix, hand) = 
      if ix == playerIx then delete c hand
      else hand

updatePlayerHand :: Card -> Model -> Model
updatePlayerHand c m = 
  Control.Lens.set (ix playerIx) (playCard c playerHand) playerHand
  where 
    playerIx = fromEnum (m ^. mPlayerTurn)
    playerHand = (m ^. mPlayerHands) !! playerIx

    
playCard :: Card -> [Card] -> [Card]
playCard = delete 

getGameState :: [[Card]] -> GameState 
getGameState ([]:[]:_) = Won True 
getGameState _ = Running

getNextPlayer :: Player -> Player 
getNextPlayer = not
