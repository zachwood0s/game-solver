module Messages 
  ( Msg(..), Tab(..)
  )
where

import qualified Solvers.Messages (Msg)

data Tab 
  = Player1
  | Player2
  | Game
  deriving (Show, Eq)

data Msg
  = NoOp
  | SolverMessagePlayer1 Solvers.Messages.Msg
  | SolverMessagePlayer2 Solvers.Messages.Msg
  | ChangeSidebarTab Tab
  deriving (Show)