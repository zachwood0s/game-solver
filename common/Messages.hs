module Messages 
  ( Msg(..), Tab(..), PlayerOptionsMsg(..)
  )
where

import qualified Network.URI as Network

import qualified Shared.Checkbox (Msg)
import qualified Solvers.Messages (Msg)
import qualified Games.TicTacToe.Messages (Msg)

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
  | TicTacToe Games.TicTacToe.Messages.Msg
  | SaveOptions Tab  
  | ChangeURI !Network.URI
  | HandleURIChange !Network.URI
  deriving (Show)