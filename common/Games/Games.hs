module Games.Games where 

import Data.Map as M

import Games.Types
import qualified Games.TicTacToe as TicTacToe
import qualified Games.Connect4 as Connect4

emptyGameModels :: M.Map Games (Game action)
emptyGameModels = 
  M.fromList 
    [ (TicTacToeGame, TicTacToe.ticTacToe $ TicTacToe.emptyModel (5, 6) 4)
    , (Connect4Game, Connect4.connect4 $ Connect4.emptyModel (6, 7) 4)
    ]