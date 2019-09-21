module Messages 
  ( Msg(..), Tab(..), PlayerOptionsMsg(..)
  )
where

import qualified Solvers.Messages (Msg)

data PlayerOptionsMsg 
  = SolverMessage Solvers.Messages.Msg
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
  deriving (Show)