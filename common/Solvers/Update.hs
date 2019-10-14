module Solvers.Update 
  (update
  ) where

import Miso (Effect(..))
import Data.Bifunctor

import Solvers.Messages (Msg(..))
import Solvers.Model (Options(..))
import qualified Shared.DropDown (update)

update :: Msg -> Options -> Effect Msg Options
update (SolverDropDownMsg msg) m@Options{solverDropDown=dropDown} =
  bimap SolverDropDownMsg updateModel (Shared.DropDown.update msg dropDown)
  where 
    updateModel dropDownModel = m {solverDropDown = dropDownModel}

