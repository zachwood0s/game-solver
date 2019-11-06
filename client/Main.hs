{-# LANGUAGE CPP               #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE PackageImports #-}

module Main where

import Data.Proxy ( Proxy(..) )
import Control.Lens ( (^.), (+=), (-=), (.=), makeLenses, zoom, use, uses)
import qualified Servant.API as Servant
import Servant.API ( (:<|>)(..) )
#if MIN_VERSION_servant(0,10,0)
import qualified Servant.Links as Servant
#endif
import qualified Miso
import Miso ( View, App(..) )
import qualified Miso.String as Miso
import Debug.Trace
import Data.Map (keys)
import qualified Data.Map as M
import Control.Monad.Reader (runReader)
import Control.Monad.Writer.Lazy (tell, lift, liftIO)

import Common
import Solvers
import "common" Solvers
import qualified Solvers.Types
import qualified Shared.Checkbox
import qualified "common" Shared.Checkbox
import qualified Games.Types as Games
import qualified Games.Games as Games

main :: IO ()
main =
  Miso.miso $ \currentURI -> App
    { initialAction = NoOp
    , model         = emptyModel currentURI
    , Miso.update   = Miso.fromTransition . updateModel
    , view          = viewModel
    , events        = Miso.defaultEvents
    , subs          = [ Miso.uriSub HandleURIChange ]
    , mountPoint    = Nothing
    }

updateModel :: Msg
            -> Miso.Transition Msg Model ()
updateModel action =
  case action of
    NoOp          -> pure ()
    ChangeURI uri ->
      Miso.scheduleIO $ do
        Miso.pushURI uri
        pure Common.NoOp
    HandleURIChange uri -> Common.mUri .= uri
    Player1Options msg -> 
      zoom mPlayer1Options $ updatePlayerOptions Player1Options msg 
    Player2Options msg -> 
      zoom mPlayer2Options $ updatePlayerOptions Player2Options msg 
    ChangeSidebarTab tab -> 
      mSelectedTab .= tab
    SaveOptions Player1 -> do
      v <- use (mPlayer1Options . mModifiedOptions)
      (mPlayer1Options . mSavedOptions) .= v
    SaveOptions Player2 -> do
      v <- use (mPlayer2Options . mModifiedOptions)
      (mPlayer2Options . mSavedOptions) .= v
    StartGame game -> case M.lookup game Games.emptyGameModels of 
      Just empty -> 
        mGame .= trace ("Starting " ++ show game) Just empty
      Nothing -> trace ("Couldn't find " ++ show game) pure ()
    GameMsg msg -> do
      m <- use mGame
      config <- playerConfig
      case m of 
        Just game -> 
          let (g, actions) = Games.updateGame config iGame msg game
          in do 
            tell $ map (\a sink -> liftIO . sink =<< a) actions
            mGame .= Just g
        Nothing -> pure ()
    where 
      solver = mSavedOptions . mSolverOptions 
      playerConfig = do 
        player1 <- use (mPlayer1Options . solver)
        player2 <- use (mPlayer2Options . solver)
        return $ Games.PlayerConfig player1 player2
      
      
updatePlayerOptions :: (PlayerOptionsMsg -> Msg) 
                    -> PlayerOptionsMsg 
                    -> Miso.Transition Msg (OptionsTab PlayerOptions) ()
updatePlayerOptions passAction msg = case msg of 
  Checkbox subMsg -> 
    zoom mModifiedOptions $ do
      zoom mComputerCheckbox $ Shared.Checkbox.update (iCheckbox passAction) subMsg
      v <- use (mComputerCheckbox . Shared.Checkbox.mChecked)
      if v then 
        mSolverOptions .= Just (Solvers.Types.emptyOptions $ map Miso.toMisoString (keys solverMap))
      else 
        mSolverOptions .= Nothing
  Solver subMsg ->  
    zoom (mModifiedOptions . mSolverOptions . traverse) $ 
      Solvers.update (iSolver passAction) subMsg


