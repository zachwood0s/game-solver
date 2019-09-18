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

view :: Model -> View Msg
view model =
  div_
    []
    [ staticCss
    , sidebar model
    ]

staticCss :: View Msg
staticCss = 
  link_ 
    [ rel_ "stylesheet"
    , href_ "static/css/main.css"
    ]

sidebar :: Model -> View Msg
sidebar Model{..} = 
  div_ 
    [ id_ "sidebar" 
    ]
    [ newGameButton
    , Solvers.View.viewOptions solverOptions
    ]

newGameButton :: View Msg
newGameButton =
  div_ 
    [ id_ "newGameButton" ]
    [ text "New game" ]

