{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where

import Miso
import Miso.String (MisoString(..))

import View (view)
import Update (update)
import Messages (Msg(..))
import Model (Model(..), emptyModel)

main :: IO ()
main = startApp App {..}
  where 
    initialAction = NoOp
    model = emptyModel
    update = Update.update
    view = View.view
    events = defaultEvents
    subs = []
    mountPoint = Nothing

--startGame (newGame (6, 5) 4)
{-main = do
  let x = minimaxAB X $ buildTree (-1) $ newGame (3, 3) 3
  print "Result"
  print x
  -}

