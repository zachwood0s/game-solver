{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Shared.Checkbox 
  ( Msg(..), Model(..)
  , Shared.Checkbox.view --Shared.Checkbox.update
  , emptyCheckbox, Interface(..)
  , mChecked, mTitle
  ) where

import Miso
import Miso.String (MisoString)
import Control.Lens ((^.), makeLenses)

data Msg 
  = Check 
  | NoOp deriving Show

data Interface action = Interface 
  { passAction :: Msg -> action 
  }

data Model = Model
  { _mTitle :: MisoString 
  , _mChecked :: Bool
  } deriving Eq

makeLenses ''Model

view :: Interface action -> Model -> View action
view iface m =
  div_ 
    [ classList_ 
      [ ("checkbox", True)
      , ("checked", m ^. mChecked)
      ]
    , onClick $ passAction iface Check
    ]
    [ h2_ [] [ text (m ^. mTitle)]
    , div_ [ class_ "box"] []
    ]

emptyCheckbox :: MisoString -> Model
emptyCheckbox title = 
  Model { _mTitle = title, _mChecked = False }