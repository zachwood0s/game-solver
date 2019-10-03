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
import qualified TicTacToe.Update 

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
update (TicTacToe msg) m@Model{game=Just (TicTacToeGame tic)} =
  let 
    solver x = (solverOptions . savedOptions . x) m
    p1Solver = solver player1Options
    p2Solver = solver player2Options
  in
    bimap TicTacToe 
      (asGameIn m . Just . TicTacToeGame) 
      (TicTacToe.Update.update p1Solver p2Solver msg tic)
update (SaveOptions Player1) m = noEff $ saveOptions (player1Options m) (asPlayer1OptionsIn m)
update (SaveOptions Player2) m = noEff $ saveOptions (player2Options m) (asPlayer2OptionsIn m)
update (SaveOptions Game) m = noEff m
update _ m = noEff m

saveOptions :: OptionsTab a -> (OptionsTab a -> Model) -> Model
saveOptions options setter = setter . setSavedOptions (modifiedOptions options) $ options

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