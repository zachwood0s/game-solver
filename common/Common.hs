{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE CPP                        #-}

module Common where

import Control.Lens
import Data.Proxy ( Proxy(..) )
import qualified Servant.API as Servant
import Servant.API ( (:<|>)(..), (:>) )
#if MIN_VERSION_servant(0,10,0)
import qualified Servant.Links as Servant
#endif
import qualified Miso
import Miso ( View )
import Miso.Html
import qualified Miso.String as Miso
import qualified Network.URI as Network

import Model
import Messages


-- Holds a servant route tree of `View action`
type ViewRoutes = Home 

-- Home route, contains two buttons and a field
type Home = View Msg


-- Checks which URI is open and shows the appropriate view
viewModel :: Model -> View Msg
viewModel m =
    case Miso.runRoute (Proxy @ViewRoutes) viewTree _mUri m of
      Left _routingError -> page404View
      Right v -> v

-- Servant tree of view functions
-- Should follow the structure of ViewRoutes
viewTree :: Model -> View Msg
viewTree = homeView 

-- View function of the Home route
homeView :: Model -> View Msg
homeView m =
  let 
    body =
      [ div_ [] [text "hello"] --sidebar model 
      --, displayGame (game model)
      ] 
  in
    div_ [] body

page404View :: View Msg
page404View =
    text "Yo, 404, page unknown. Go to /. Shoo!"

-- Network.URI that points to the home route
homeLink :: Network.URI
homeLink =
#if MIN_VERSION_servant(0,10,0)
    Servant.linkURI $ Servant.safeLink (Proxy @ViewRoutes) (Proxy @Home)
#else
    safeLink (Proxy @ViewRoutes) (Proxy @Home)
#endif
