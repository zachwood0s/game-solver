module Model 
  ( Model(..), emptyModel
  , OptionsTab(..)
  ) 
where

import Data.Map

import qualified Solvers.Model
import Solvers (solverMap)
import Messages (Msg, Tab(..))

data OptionsTab a = OptionsTab 
  { savedOptions :: a
  , modifiedOptions :: a
  }
  deriving Eq

data Model = Model
  { player1Options :: OptionsTab (Maybe Solvers.Model.Options)
  , player2Options :: OptionsTab (Maybe Solvers.Model.Options)
  , selectedTab :: Tab
  }
  deriving Eq


emptyModel :: Model 
emptyModel = 
  Model 
    { player1Options = OptionsTab Nothing (Just $ Solvers.Model.emptyOptions (keys solverMap))
    , player2Options = OptionsTab Nothing Nothing
    , selectedTab = Player1
    }