module Model 
  ( Model(..), emptyModel
  ) 
where

import Data.Map

import qualified Solvers.Model
import Solvers (solverMap)

data Model = Model
  { solverOptions :: Solvers.Model.Options
  }
  deriving Eq

emptyModel :: Model 
emptyModel = 
  Model 
    { solverOptions = Solvers.Model.emptyOptions (keys solverMap)
    }