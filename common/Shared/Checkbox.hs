{-# LANGUAGE OverloadedStrings #-}

module Shared.Checkbox 
  ( Msg(..), Model(..)
  , Shared.Checkbox.view --Shared.Checkbox.update
  , emptyCheckbox
  ) where

import Miso
import Miso.String (MisoString)

data Msg = Check deriving Show

data Model = Model
  { title :: MisoString 
  , checked :: Bool
  } deriving Eq

{-
update :: Msg -> Model -> Effect Msg Model
update Check (Model t checked) = noEff $ Model t (not checked)
-}

view :: Model -> View Msg
view (Model title checked) =
  div_ 
    [ classList_ 
      [ ("checkbox", True)
      , ("checked", checked)
      ]
    , onClick Check
    ]
    [ h2_ [] [ text title]
    , div_ [ class_ "box"] []
    ]

emptyCheckbox :: MisoString -> Model
emptyCheckbox title = 
  Model { title = title, checked = False }