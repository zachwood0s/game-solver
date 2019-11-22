{-# LANGUAGE PackageImports #-}

module Shared.Checkbox 
  ( update
  ) where
import Control.Lens ((.=), (^.), use)

import "common" Shared.Checkbox
import qualified Miso


update :: Interface action 
       -> Msg 
       -> Miso.Transition action Model ()
update iface msg = case msg of 
  Check -> do
    value <- use mChecked
    mChecked .= not value

  NoOp -> pure ()