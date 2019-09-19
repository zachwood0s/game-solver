module Update 
  ( Update.update
  )
where

import Miso
import Data.Bifunctor

import Messages
import Model
import qualified Solvers.Update (update)

update :: Msg -> Model -> Effect Msg Model
update NoOp m = noEff m
update (SolverMessage msg) m@Model{solverOptions=options} =
  bimap SolverMessage updateModel (Solvers.Update.update msg options)
  where
    updateModel solverModel = m{solverOptions = solverModel}