{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TicTacToe.View
  ( TicTacToe.View.view
  ) where

import Miso
import Miso.String (toMisoString)
import qualified Data.Map as M

import TicTacToe.Model
import Messages

view :: Model -> View Msg
view m = 
  drawBoard m

drawBoard :: Model -> View Msg
drawBoard Model{..} = 
  table_ 
    [ id_ "board" 
    ]
    $ map drawRow board
  where
    (rows, cols) = boardSize
    width = 100 `quot` cols
    drawRow row = div_ [ class_ "row" ] $ map drawCol row
    drawCol Nothing = drawCell []
    drawCol (Just True) = drawCell [text "x"]
    drawCol (Just False) = drawCell [text "o"]
    drawCell = 
      div_ 
        [ class_ "col"
        , style_ $ M.singleton "width" $ toMisoString (show width ++ "%")
        ]
        

