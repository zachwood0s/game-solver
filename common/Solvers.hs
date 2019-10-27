{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Solvers 
  ( solverMap, runSolverFromString, viewOptions
  , iDropdown
  ) where

import Data.Map
import Miso 
import Control.Lens ((^.))

import Solvers.Minimax
import Solvers.MinimaxCached
import Solvers.Types
import Solvers.Utils
import qualified Shared.DropDown


solverMap :: Map String (SolverFunc a b)
solverMap =
  fromList 
    [ ("Minimax", minimax)
    --  ("Alpha Beta", minimaxAB)
    , ("Minimax Cached", minimaxCached)
    --, ("Alpha Cached", minimaxABCached)
    --, ("MTD(f)", mtdf)
    --, ("MTD(f) w/ Memory", mtdfMem)
    ]

runSolverFromString :: String -> Solver a b -> Bool -> Int -> a -> SolverResult b
runSolverFromString name s maxPlayer depth game =
  case Data.Map.lookup name solverMap of 
    Just strategy -> runSolver strategy s maxPlayer depth game
    Nothing -> buildNode s (Exact 0) game




-- VIEW --
viewOptions :: Interface action -> Solvers.Types.Options -> Miso.View action
viewOptions iface o = 
  section_
    [] 
    [ h1_ [] [ text "Solver" ]
    , Shared.DropDown.view (iDropdown iface) (o ^. mSolverDropDown)
    ]


iDropdown :: Interface action -> Shared.DropDown.Interface action 
iDropdown iface = 
  Shared.DropDown.Interface 
    { passAction = passAction iface . DropDown 
    }

{-
-- UPDATE -- 
update :: Msg -> Options -> Effect Msg Options
update (SolverDropDownMsg msg) m@Options{solverDropDown=dropDown} =
  bimap SolverDropDownMsg updateModel (Shared.DropDown.update msg dropDown)
  where 
    updateModel dropDownModel = m {solverDropDown = dropDownModel}
    -}

