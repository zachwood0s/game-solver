module Model 
  ( Model(..), CurrentOptions
  ) 
where

import Miso.String (MisoString)

import TicTacToe.Model (TicTacToeOptions)

data Model = Model
  { string :: MisoString
  }
  deriving (Show, Eq)


type CurrentOptions = TicTacToeOptions