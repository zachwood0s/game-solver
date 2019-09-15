module Solvers.View 
  ( ) where

import Miso

import Messages
import Solvers.Types

viewOptions :: SolverOptions a b -> View Msg
viewOptions _ = _div [] []