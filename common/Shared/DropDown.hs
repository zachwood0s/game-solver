{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Shared.DropDown
  ( Model(..), Msg(..)
  , Shared.DropDown.view, Interface(..)
  , mExpanded, mSelected
  ) where

import Miso
import Miso.String (MisoString)
import Control.Lens ((^.), makeLenses)

data Msg 
  = Toggle 
  | Select MisoString
  deriving Show

data Model = Model 
  { _mTitle :: MisoString
  , _mSelected :: MisoString 
  , _mOptions :: [MisoString] 
  , _mExpanded :: Bool
  } deriving (Eq)

data Interface action = Interface 
  { passAction :: Msg -> action 
  }

makeLenses ''Model

view :: Interface action -> Model -> View action
view iface m =
  div_ 
    [ classList_ 
      [ ("dropDown", True)
      , ("open", m ^. mExpanded)
      ]
    , onClick $ passAction iface Toggle
    ]
    [ h2_ [] [ text (m ^. mTitle) ]
    , h3_ [] [ text (m ^. mSelected) ]
    , ul_ [] (map createListItem (m ^. mOptions))
    ]
  where 
    createListItem item = 
      li_ 
        [ onClick $ passAction iface (Select item) 
        , classList_
          [ ("selected", item == (m ^. mSelected))
          ]
        ] 
        [ text item ]
