{-# LANGUAGE RecordWildCards #-}

module Update 
  ( Update.update
  )
where

import Miso
import Data.Bifunctor
import Data.Map

import Messages
import Model
import Solvers (solverMap)
import qualified Shared.Checkbox (update)
import qualified Solvers.Update (update)
import qualified Solvers.Model (Options, emptyOptions)

update :: Msg -> Model -> Effect Msg Model
update (Player1Options msg) m@Model{..} =
  bimap Player1Options 
    (asPlayer1OptionsIn m)
    (updatePlayerOptions msg player1Options)
update (Player2Options msg) m@Model{..} =
  bimap Player2Options 
    (asPlayer2OptionsIn m)
    (updatePlayerOptions msg player2Options)
update (ChangeSidebarTab tab) m =
  noEff m{selectedTab = tab};
update _ m = noEff m

updatePlayerOptions :: PlayerOptionsMsg -> OptionsTab PlayerOptions -> Effect PlayerOptionsMsg (OptionsTab PlayerOptions)
updatePlayerOptions Save m = noEff m
updatePlayerOptions (Solver subMsg) m@OptionsTab{..}
  | Just x <- solverOptions modifiedOptions =
    bimap Solver
      (asModifiedOptionsIn m . asSolverOptionsIn modifiedOptions . Just)
      (Solvers.Update.update subMsg x)
  | otherwise = noEff m
updatePlayerOptions (Checkbox subMsg) m@OptionsTab{..} =
  bimap Checkbox 
    (asModifiedOptionsIn m . setSolver . setCheckbox)
    (Shared.Checkbox.update subMsg $ computerCheckbox modifiedOptions)
  where 
    setSolver = setSolverOptions $ newSolverOptions (solverOptions modifiedOptions)
    setCheckbox = asComputerCheckboxIn modifiedOptions
    newSolverOptions (Just _) = Nothing
    newSolverOptions Nothing = Just $ Solvers.Model.emptyOptions (keys solverMap)
updatePlayerOptions _ m = noEff m

{- 
updater . setModifiedOptions . Just (updated)
-}