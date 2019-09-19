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
sidebar Model{..} = 
  div_ 
    [ id_ "sidebar" 
    ]
    [ newGameButton
    , SolverMessage <$> Solvers.View.viewOptions solverOptions
    ]

newGameButton :: View Msg
newGameButton =
  div_ 
    [ id_ "newGameButton" ]
    [ text "New game" ]

