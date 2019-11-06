module Games.Games where 

import Data.Map as M

import Games.Types
import qualified Games.TicTacToe as TicTacToe

emptyGameModels :: M.Map Games (Game action)
emptyGameModels = 
  M.fromList 
    [ (TicTacToeGame, TicTacToe.ticTacToe $ TicTacToe.emptyModel (5, 6) 4)
    ]