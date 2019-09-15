{-# LANGUAGE OverloadedStrings          #-}

module View 
  ( View.view 
  )
where

import Miso

import Messages
import Model

view :: Model -> View Msg
view Model{string=s} =
  div_
    []
    [ staticCss
    , sidebar
    , text s 
    ]

staticCss :: View Msg
staticCss = 
  link_ 
    [ rel_ "stylesheet"
    , href_ "static/css/main.css"
    ]

sidebar :: View Msg
sidebar = 
  div_ 
    [ id_ "sidebar" 
    ]
    [ newGameButton
    , solverSection
    ]

newGameButton :: View Msg
newGameButton =
  div_ 
    [ id_ "newGameButton" ]
    [ text "New game" ]

solverSection :: View Msg
solverSection =
  section_
    []
    [ h1_ [] [ text "Solver" ]
    ]
