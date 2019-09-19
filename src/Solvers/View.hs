{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Solvers.View 
  ( viewOptions 
  ) where

import Miso

import Solvers.Messages 
import qualified Solvers.Model  
import qualified Shared.DropDown (view)

viewOptions :: Solvers.Model.Options -> View Msg
viewOptions Solvers.Model.Options{..} = 
  section_
    [] 
    [ h1_ [] [ text "Solver" ]
    , SolverDropDownMsg <$> Shared.DropDown.view solverDropDown
    ]
  
    
