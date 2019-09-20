module Model 
  ( Model(..), emptyModel
  ) 
where

import Data.Map

import qualified Solvers.Model
import Solvers (solverMap)
import Messages (Tab(..))

data Model = Model
  { solverOptionsPlayer1 :: Maybe Solvers.Model.Options
  , solverOptionsPlayer2 :: Maybe Solvers.Model.Options
  , selectedTab :: Tab
  }
  deriving Eq

emptyModel :: Model 
emptyModel = 
  Model 
    { solverOptionsPlayer1 = Just (Solvers.Model.emptyOptions (keys solverMap))
    , solverOptionsPlayer2 = Nothing
    , selectedTab = Player1
    }