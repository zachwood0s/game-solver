{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE RecordWildCards            #-}

module Common where

import Control.Lens
import Data.Proxy ( Proxy(..) )
import qualified Servant.API as Servant
import Servant.API ( (:<|>)(..), (:>) )
#if MIN_VERSION_servant(0,10,0)
import qualified Servant.Links as Servant
#endif
import qualified Miso
import Miso ( View )
import Miso.Html
import qualified Miso.String as Miso
import qualified Network.URI as Network

import qualified Games.TicTacToe
import qualified Shared.Checkbox
import qualified Solvers
import qualified Solvers.Types
import qualified Games.Types as Games

data PlayerOptionsMsg 
  = Solver Solvers.Types.Msg
  | Checkbox Shared.Checkbox.Msg
  deriving (Show)

data Tab 
  = Player1
  | Player2
  | Game
  deriving (Show, Eq)

data Msg
  = NoOp
  | StartGame Games.Games
  | GameMsg Games.Msg
  | Player1Options PlayerOptionsMsg
  | Player2Options PlayerOptionsMsg
  | ChangeSidebarTab Tab
  | SaveOptions Tab  
  | ChangeURI !Network.URI
  | HandleURIChange !Network.URI
  deriving (Show)


-- Holds a servant route tree of `View action`
type ViewRoutes = Home 

-- Home route, contains two buttons and a field
type Home = View Msg


data OptionsTab a = OptionsTab 
  { _mSavedOptions :: !a 
  , _mModifiedOptions :: !a 
  } deriving Eq

data Model = Model
  { _mUri :: !Network.URI
  , _mPlayer1Options :: OptionsTab PlayerOptions
  , _mPlayer2Options :: OptionsTab PlayerOptions
  , _mGame :: Maybe (Games.Game Msg)
  , _mSelectedTab :: !Tab
  } deriving (Eq)

data PlayerOptions = PlayerOptions 
  { _mComputerCheckbox :: Shared.Checkbox.Model 
  , _mSolverOptions :: Maybe Solvers.Types.Options
  } deriving Eq

makeLenses ''OptionsTab
makeLenses ''Model
makeLenses ''PlayerOptions

emptyModel :: Network.URI -> Model 
emptyModel uri = 
  Model 
    { _mUri = uri
    , _mPlayer1Options = OptionsTab emptyPlayerOptions emptyPlayerOptions
    , _mPlayer2Options = OptionsTab emptyPlayerOptions emptyPlayerOptions
    , _mSelectedTab = Player1
    , _mGame = Nothing
    }

emptyPlayerOptions :: PlayerOptions
emptyPlayerOptions = 
  PlayerOptions
    { _mComputerCheckbox = Shared.Checkbox.emptyCheckbox "Computer Player"
    , _mSolverOptions = Nothing
    }


-- Checks which URI is open and shows the appropriate view
viewModel :: Model -> View Msg
viewModel m =
    case Miso.runRoute (Proxy @ViewRoutes) viewTree _mUri m of
      Left _routingError -> page404View
      Right v -> v

-- Servant tree of view functions
-- Should follow the structure of ViewRoutes
viewTree :: Model -> View Msg
viewTree = homeView 

-- View function of the Home route
homeView :: Model -> View Msg
homeView m =
  let 
    body =
      [ sidebar m 
      , gameView
      ] 
  in
    div_ [] body
  where 
    gameView = case m ^. mGame of 
      Just g -> Games.view g iGame
      Nothing -> text ""

page404View :: View Msg
page404View =
    text "Yo, 404, page unknown. Go to /. Shoo!"

-- Network.URI that points to the home route
homeLink :: Network.URI
homeLink =
#if MIN_VERSION_servant(0,10,0)
    Servant.linkURI $ Servant.safeLink (Proxy @ViewRoutes) (Proxy @Home)
#else
    safeLink (Proxy @ViewRoutes) (Proxy @Home)
#endif



sidebar :: Model -> View Msg
sidebar m@Model{..} = 
  div_ 
    [ id_ "sidebar" 
    ]
    [ newGameButton
    , viewTabs m
    , viewTab m
    ]

newGameButton :: View Msg
newGameButton =
  div_ 
    [ id_ "newGameButton"
    , onClick (StartGame Games.Connect4Game) 
    ]
    [ text "New game" ]

viewTabs :: Model -> View Msg 
viewTabs m= 
  div_ 
    [ id_ "sidebarTabs" ]
    [ createTab "icon-user" Player1
    , createTab "icon-users" Player2
    , createTab "icon-dice" Game
    ]
  where 
    createTab icon t =
      div_ 
        [ classList_
          [ ("option", True)
          , ("selected", (m ^. mSelectedTab) == t)
          ]
        , onClick (ChangeSidebarTab t)
        ]
        [ span_ [class_ icon ] [] ]

viewTab :: Model -> View Msg 
viewTab Model{_mSelectedTab=Game, ..} = text ""
viewTab Model{_mSelectedTab=Player1, ..} 
  = viewOptions _mPlayer1Options (viewPlayerTab Player1Options) Player1
viewTab Model{_mSelectedTab=Player2, ..} 
  = viewOptions _mPlayer2Options (viewPlayerTab Player2Options) Player2

    
viewOptions :: OptionsTab a -> (a -> View Msg) -> Tab -> View Msg
viewOptions o viewFunc tab = 
  div_ 
    []
    [ viewFunc (o ^. mModifiedOptions)
    , div_ 
      [ class_ "saveButton"
      , onClick (SaveOptions tab)
      ]
      [ text "Save Changes"
      ]
    ]

viewPlayerTab :: (PlayerOptionsMsg -> Msg) -> PlayerOptions -> View Msg
viewPlayerTab msg m = 
  div_ 
    [] 
    [ section_ 
      [] 
      [ h1_ [] [ text "Player" ]
      , Shared.Checkbox.view (iCheckbox msg) (m ^. mComputerCheckbox)
      ]
    , body (m ^. mSolverOptions)
    ]
  where 
    body Nothing = text ""
    body (Just o) = Solvers.viewOptions (iSolver msg) o


iCheckbox :: (PlayerOptionsMsg -> Msg) -> Shared.Checkbox.Interface Msg
iCheckbox msg = 
  Shared.Checkbox.Interface 
    { passAction = msg . Checkbox
    }

iSolver :: (PlayerOptionsMsg -> Msg) -> Solvers.Types.Interface Msg
iSolver msg =
  Solvers.Types.Interface 
    { passAction = msg . Solver 
    }

iGame :: Games.Interface Msg 
iGame = 
  Games.Interface
    { passAction = GameMsg
    }