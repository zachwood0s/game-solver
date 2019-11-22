module Games.Messages.TicTacToe where 

data Msg 
  = Move Int Int
  deriving (Show, Eq)
