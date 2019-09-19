module Solvers.Messages 
  ( Msg(..)
  ) where

import qualified Shared.DropDown (Msg)

data Msg 
  = SolverDropDownMsg Shared.DropDown.Msg
  deriving Show