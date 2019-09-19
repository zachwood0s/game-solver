module Messages 
  ( Msg(..)
  )
where

import qualified Solvers.Messages (Msg)

data Msg
  = NoOp
  | SolverMessage Solvers.Messages.Msg
  deriving Show