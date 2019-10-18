{-# LANGUAGE CPP               #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import qualified Common
import Data.Proxy ( Proxy(..) )
import Control.Lens ( (^.), (+=), (-=), (.=), makeLenses )
import qualified Servant.API as Servant
import Servant.API ( (:<|>)(..) )
#if MIN_VERSION_servant(0,10,0)
import qualified Servant.Links as Servant
#endif
import qualified Miso
import Miso ( View, App(..) )
import qualified Miso.String as Miso

import Messages
import Model

main :: IO ()
main =
  Miso.miso $ \currentURI -> App
    { initialAction = NoOp
    , model         = emptyModel currentURI
    , update        = Miso.fromTransition . updateModel
    , view          = Common.viewModel
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
          pure NoOp
      HandleURIChange uri -> mUri .= uri
