
module Shared.Checkbox 
  ( update
  ) where
import Control.Lens ((.=), (^.))

import Shared.Checkbox
import qualified Miso


update :: Interface action 
       -> Msg 
       -> Miso.Transition action Model ()
update iface msg = case msg of 
  Check -> 
    mChecked .= not mChecked

  NoOp -> pure ()