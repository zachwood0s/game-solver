module Messages 
  ( Msg(..), Tab(..), PlayerOptionsMsg(..)
  )
where

import qualified Shared.Checkbox (Msg)
import qualified Solvers.Messages (Msg)
import qualified TicTacToe.Messages (Msg)

data PlayerOptionsMsg 
  = Solver Solvers.Messages.Msg
  | Checkbox Shared.Checkbox.Msg
  | Save
  deriving (Show)

data Tab 
  = Player1
  | Player2
  | Game
  deriving (Show, Eq)

data Msg
  = NoOp
  | Player1Options PlayerOptionsMsg
  | Player2Options PlayerOptionsMsg
  | ChangeSidebarTab Tab
  | TicTacToe TicTacToe.Messages.Msg
  | SaveOptions Tab
  deriving (Show)