module Update 
  ( Update.update
  )
where

import Miso

import Messages
import Model

update :: Msg -> Model -> Effect Msg Model
update NoOp = noEff