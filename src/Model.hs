module Model 
  ( Model(..), Options(..)
  ) 
where

import Miso.String (MisoString)

import qualified TicTacToe.Model (TicTacToeOptions)

data Model = Model
  { string :: MisoString
  , options :: Options
  }
  deriving Eq


data Options = TicTacToeOptions TicTacToe.Model.TicTacToeOptions
  deriving Eq