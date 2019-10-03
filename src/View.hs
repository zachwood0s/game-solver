{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module View 
  ( View.view 
  )
where

import Miso

import Messages
import Model
import qualified Shared.Checkbox (view)
import qualified Solvers.View (viewOptions)
import qualified Solvers.Model (Options(..))
import qualified Solvers.Messages (Msg(..))
import qualified TicTacToe.View

view :: Model -> View Msg
view model =
  let 
    body =
      [ sidebar model 
      , displayGame (game model)
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

displayGame :: Maybe Game -> View Msg
displayGame Nothing = text ""
displayGame (Just (TicTacToeGame m )) = TicTacToe <$> TicTacToe.View.view m


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
  = viewOptions player1Options (viewPlayerTab Player1Options) Player1
viewTab Model{selectedTab=Player2, ..} 
  = viewOptions player2Options (viewPlayerTab Player2Options) Player2

    
viewOptions :: OptionsTab a -> (a -> View Msg) -> Tab -> View Msg
viewOptions OptionsTab{modifiedOptions=o} viewFunc tab = 
  div_ 
    []
    [ viewFunc o 
    , div_ 
      [ class_ "saveButton"
      , onClick (SaveOptions tab)
      ]
      [ text "Save Changes"
      ]
    ]

viewPlayerTab :: (PlayerOptionsMsg -> Msg) -> PlayerOptions -> View Msg
viewPlayerTab msg PlayerOptions{..} = 
  div_ 
    [] 
    [ section_ 
      [] 
      [ h1_ [] [ text "Player" ]
      , msg . Checkbox <$> Shared.Checkbox.view computerCheckbox 
      ]
    , msg . Solver <$> body solverOptions
    ]
  where 
    body Nothing = text ""
    body (Just o) = Solvers.View.viewOptions o


viewSolverOptions :: (Solvers.Messages.Msg -> Msg) -> Maybe Solvers.Model.Options -> View Msg
viewSolverOptions _ Nothing = text ""
viewSolverOptions msg (Just options) =
  msg <$> Solvers.View.viewOptions options
  

