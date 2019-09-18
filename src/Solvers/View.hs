{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Solvers.View 
  ( viewOptions 
  ) where

import Miso

import Messages
import qualified Solvers.Model  

viewOptions :: Solvers.Model.Options -> View Msg
viewOptions model = 
  section_
    [] 
    [ h1_ [] [ text "Solver" ]
    , solverDropDown model
    ]

solverDropDown :: Solvers.Model.Options -> View Msg 
solverDropDown Solvers.Model.Options{..} = 
  div_ 
    [ class_ "dropDown" ]
    [ h2_ [] [ text "Algorithm" ]
    , h3_ [] [ text selectedSolverFunc ]
    , ul_ [] (map createListItem solverList)
    ]
  where
    createListItem item = li_ [] [ text item ]
  
    
