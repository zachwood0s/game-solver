{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards #-}

module View 
  ( View.view 
  )
where

import Miso

import Messages
import Model
import qualified Solvers.View (viewOptions)
import qualified Solvers.Model (Options(..))
import qualified Solvers.Messages (Msg(..))

view :: Model -> View Msg
view model =
  let 
    body =
      [ sidebar model 
      ] ++ staticCss
  in
    div_ [] body


staticCss :: [View Msg]
staticCss = 
  [ link_ 
    [ rel_ "stylesheet"
    , href_ "static/css/main.css"
    ]
  , link_
    [ rel_ "stylesheet"
    , href_ "static/css/fonts.css"
    ]
  ]

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
    [ id_ "newGameButton" ]
    [ text "New game" ]

viewTabs :: Model -> View Msg 
viewTabs Model{selectedTab=tab}= 
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
          , ("selected", tab == t)
          ]
        , onClick (ChangeSidebarTab t)
        ]
        [ span_ [class_ icon ] [] ]

viewTab :: Model -> View Msg 
viewTab Model{selectedTab=Game, ..} = text ""
viewTab Model{selectedTab=Player1, ..} 
  = viewOptions player1Options (viewPlayerTab Player1Options)
viewTab Model{selectedTab=Player2, ..} 
  = viewOptions player2Options (viewPlayerTab Player2Options)
    
viewOptions :: OptionsTab a -> (a -> View Msg) -> View Msg
viewOptions OptionsTab{modifiedOptions=o} viewFunc = 
  viewFunc o 

viewPlayerTab :: (PlayerOptionsMsg -> Msg) -> Maybe Solvers.Model.Options -> View Msg
viewPlayerTab msg options = 
  div_ 
    [] 
    [ body options ]
  where 
    body Nothing = text ""
    body (Just o) = msg . SolverMessage <$> Solvers.View.viewOptions o


viewSolverOptions :: (Solvers.Messages.Msg -> Msg) -> Maybe Solvers.Model.Options -> View Msg
viewSolverOptions _ Nothing = text ""
viewSolverOptions msg (Just options) =
  msg <$> Solvers.View.viewOptions options
  

