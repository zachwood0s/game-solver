{-# LANGUAGE RecordWildCards #-}

module Update 
  ( Update.update
  )
where

import Miso
import Data.Bifunctor

import Messages
import Model
import qualified Solvers.Update (update)
import qualified Solvers.Model (Options)

update :: Msg -> Model -> Effect Msg Model

{-
update (Player1Options msg) m@Model{..} =
   
update (SolverMessagePlayer1 msg) m@Model{solverOptionsPlayer1=Just options} =
  bimap SolverMessagePlayer1 updateModel (Solvers.Update.update msg options)
  where
    updateModel updated = m{ solverOptionsPlayer1 = Just updated }

update (SolverMessagePlayer2 msg) m@Model{solverOptionsPlayer2=Just options} =
  bimap SolverMessagePlayer2 updateModel (Solvers.Update.update msg options)
  where
    updateModel updated = m{ solverOptionsPlayer2 = Just updated }
-}

update (ChangeSidebarTab tab) m =
  noEff m{selectedTab = tab};

update _ m = noEff m


updatePlayerOptions :: (PlayerOptionsMsg -> Msg) 
                    -> PlayerOptionsMsg 
                    -> Solvers.Model.Options 
                    -> (Solvers.Model.Options -> Model) 
                    -> Effect Msg Model

updatePlayerOptions msg (SolverMessage subMsg) options updateModel = 
  bimap (msg . SolverMessage) updateModel (Solvers.Update.update subMsg options)