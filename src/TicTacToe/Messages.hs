module TicTacToe.Messages 
  ( Msg(..)
  ) where


data Msg 
  = NoOp
  | Move Int Int
  | DoAi
  deriving Show