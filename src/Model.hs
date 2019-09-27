{-# LANGUAGE OverloadedStrings #-}

module Model 
  ( Model(..), emptyModel
  , setPlayer1Options, setPlayer2Options, asPlayer1OptionsIn, asPlayer2OptionsIn
  , OptionsTab(..)
  , setSavedOptions, setModifiedOptions, asSavedOptionsIn, asModifiedOptionsIn
  , PlayerOptions(..)
  , asSolverOptionsIn, setSolverOptions, asComputerCheckboxIn, setComputerCheckbox
  , Game(..)
  ) 
where

import Data.Map

import qualified Solvers.Model
import qualified Shared.Checkbox
import qualified TicTacToe.Model
import Solvers (solverMap)
import Messages (Msg, Tab(..))

data OptionsTab a = OptionsTab 
  { savedOptions :: a
  , modifiedOptions :: a
  } deriving Eq

data Model = Model
  { player1Options :: OptionsTab PlayerOptions
  , player2Options :: OptionsTab PlayerOptions
  , game :: Maybe Game
  , selectedTab :: Tab
  } deriving Eq

data PlayerOptions = PlayerOptions 
  { computerCheckbox :: Shared.Checkbox.Model 
  , solverOptions :: Maybe Solvers.Model.Options 
  } deriving Eq

data Game 
  = TicTacToe TicTacToe.Model.Model 
  deriving Eq

setModifiedOptions :: a -> OptionsTab a -> OptionsTab a
setModifiedOptions o m = m {modifiedOptions = o}

asModifiedOptionsIn :: OptionsTab a -> a -> OptionsTab a
asModifiedOptionsIn = flip setModifiedOptions

setSavedOptions :: a -> OptionsTab a -> OptionsTab a
setSavedOptions o m = m {savedOptions = o}

asSavedOptionsIn :: OptionsTab a -> a -> OptionsTab a
asSavedOptionsIn = flip setSavedOptions

setPlayer1Options :: OptionsTab PlayerOptions -> Model -> Model
setPlayer1Options o m = m {player1Options = o}

asPlayer1OptionsIn :: Model -> OptionsTab PlayerOptions -> Model
asPlayer1OptionsIn = flip setPlayer1Options

setPlayer2Options :: OptionsTab PlayerOptions -> Model -> Model
setPlayer2Options o m = m {player2Options = o}

asPlayer2OptionsIn :: Model -> OptionsTab PlayerOptions -> Model
asPlayer2OptionsIn = flip setPlayer2Options

emptyPlayerOptions :: PlayerOptions
emptyPlayerOptions = 
  PlayerOptions
    { computerCheckbox = Shared.Checkbox.emptyCheckbox "Computer Player"
    , solverOptions = Nothing
    }

setSolverOptions :: Maybe Solvers.Model.Options -> PlayerOptions -> PlayerOptions
setSolverOptions o m = m {solverOptions = o}

asSolverOptionsIn :: PlayerOptions -> Maybe Solvers.Model.Options -> PlayerOptions
asSolverOptionsIn = flip setSolverOptions

setComputerCheckbox :: Shared.Checkbox.Model -> PlayerOptions -> PlayerOptions
setComputerCheckbox o m = m {computerCheckbox = o}

asComputerCheckboxIn :: PlayerOptions -> Shared.Checkbox.Model -> PlayerOptions
asComputerCheckboxIn = flip setComputerCheckbox

emptyModel :: Model 
emptyModel = 
  Model 
    { player1Options = OptionsTab emptyPlayerOptions emptyPlayerOptions
    , player2Options = OptionsTab emptyPlayerOptions emptyPlayerOptions
    , selectedTab = Player1
    , game = (Just . TicTacToe) $ TicTacToe.Model.emptyModel (6, 10) 3
    }