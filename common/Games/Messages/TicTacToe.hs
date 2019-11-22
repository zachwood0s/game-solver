module Games.Messages.TicTacToe where 

data Msg 
  = Move Int Int
  | DoAi
  deriving (Show, Eq)