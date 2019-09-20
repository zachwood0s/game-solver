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
update (SolverMessagePlayer1 _) m@Model{solverOptionsPlayer1=Nothing} = noEff m
update (SolverMessagePlayer1 msg) m@Model{solverOptionsPlayer1=Just options} =
  bimap SolverMessagePlayer1 updateModel (Solvers.Update.update msg options)
  where
    updateModel updated = m{ solverOptionsPlayer1 = Just updated }

update (SolverMessagePlayer2 _) m@Model{solverOptionsPlayer2=Nothing} = noEff m
update (SolverMessagePlayer2 msg) m@Model{solverOptionsPlayer2=Just options} =
  bimap SolverMessagePlayer2 updateModel (Solvers.Update.update msg options)
  where
    updateModel updated = m{ solverOptionsPlayer2 = Just updated }

update (ChangeSidebarTab tab) m =
  noEff m{selectedTab = tab};