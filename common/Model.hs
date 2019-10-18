{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Model 
  ( Model(..), PlayerOptions(..), OptionsTab(..)
  , emptyModel, mUri
  ) where

import Control.Lens 
import qualified Network.URI as Network

import qualified Games.TicTacToe.Model
import qualified Shared.Checkbox
import qualified Solvers.Model
import Messages

data OptionsTab a = OptionsTab 
  { _mSavedOptions :: !a 
  , _mModifiedOptions :: !a 
  } deriving Eq

data Model = Model
  { _mUri :: !Network.URI
  , _mPlayer1Options :: OptionsTab PlayerOptions
  , _mPlayer2Options :: OptionsTab PlayerOptions
  , _mGame :: Maybe Game
  , _mSelectedTab :: !Tab
  } deriving Eq

data PlayerOptions = PlayerOptions 
  { _mComputerCheckbox :: Shared.Checkbox.Model 
  , _mSolverOptions :: Maybe Solvers.Model.Options
  } deriving Eq

data Game 
  = TicTacToeGame Games.TicTacToe.Model.Model
  deriving Eq

makeLenses ''Model
makeLenses ''PlayerOptions

emptyModel :: Network.URI -> Model 
emptyModel uri = 
  Model 
    { _mUri = uri
    , _mPlayer1Options = OptionsTab emptyPlayerOptions emptyPlayerOptions
    , _mPlayer2Options = OptionsTab emptyPlayerOptions emptyPlayerOptions
    , _mSelectedTab = Player1
    , _mGame = (Just . TicTacToeGame) $ Games.TicTacToe.Model.emptyModel (5, 6) 4
    }

emptyPlayerOptions :: PlayerOptions
emptyPlayerOptions = 
  PlayerOptions
    { _mComputerCheckbox = Shared.Checkbox.emptyCheckbox "Computer Player"
    , _mSolverOptions = Nothing
    }