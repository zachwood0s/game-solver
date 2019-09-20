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
  = viewSolverOptions SolverMessagePlayer1 solverOptionsPlayer1
viewTab Model{selectedTab=Player2, ..} 
  = viewSolverOptions SolverMessagePlayer2 solverOptionsPlayer2
    

viewSolverOptions :: (Solvers.Messages.Msg -> Msg) -> Maybe Solvers.Model.Options -> View Msg
viewSolverOptions _ Nothing = text ""
viewSolverOptions msg (Just options) =
  msg <$> Solvers.View.viewOptions options
  

