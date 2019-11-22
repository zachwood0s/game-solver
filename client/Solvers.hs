{-# LANGUAGE PackageImports #-}

module Solvers 
  ( update
  ) where 

import Control.Lens (zoom)
import qualified Miso

import "common" Solvers
import Solvers.Types
import qualified Shared.DropDown


update :: Interface action 
       -> Msg 
       -> Miso.Transition action Options ()
update iface msg = case msg of 
  DropDown subMsg -> 
    zoom mSolverDropDown $ Shared.DropDown.update (iDropdown iface) subMsg

