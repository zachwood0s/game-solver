{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Shared.DropDown
  ( Model(..), Msg(..)
  , Shared.DropDown.view --, Shared.DropDown.update
  ) where

import Miso
import Miso.String (MisoString)

data Msg 
  = Toggle 
  | Select MisoString
  deriving Show

data Model = Model 
  { title :: MisoString
  , selected :: MisoString 
  , options :: [MisoString] 
  , expanded :: Bool
  } deriving (Eq)


{-
update :: Msg -> Model -> Effect Msg Model
update Toggle m@Model{expanded=e} = noEff m { expanded = not e }
update (Select s) m = noEff m {selected = s}
-}

view :: Model -> View Msg
view Model{..} =
  div_ 
    [ classList_ 
      [ ("dropDown", True)
      , ("open", expanded)
      ]
    , onClick Toggle
    ]
    [ h2_ [] [ text title ]
    , h3_ [] [ text selected ]
    , ul_ [] (map createListItem options)
    ]
  where 
    createListItem item = 
      li_ 
        [ onClick (Select item) 
        , classList_
          [ ("selected", item == selected)
          ]
        ] 
        [ text item ]
