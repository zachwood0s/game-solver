module Games.Messages.Connect4 where

data Msg 
  = Move Int
  | DoAi
  deriving (Show, Eq)