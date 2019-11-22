{-# LANGUAGE PackageImports #-}

module Shared.DropDown 
  ( update
  ) where

import qualified Miso
import Control.Lens ((.=), (^.), use)

import "common" Shared.DropDown

update :: Interface action 
       -> Msg 
       -> Miso.Transition action Model ()

update iface msg = case msg of 
  Toggle -> do
    v <- use mExpanded
    mExpanded .= not v
  Select s -> 
    mSelected .= s
