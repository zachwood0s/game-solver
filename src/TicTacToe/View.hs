{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module TicTacToe.View
  ( TicTacToe.View.view
  ) where

import Utils
import Miso
import Miso.String (toMisoString)
import qualified Data.Map as M

import TicTacToe.Model
import TicTacToe.Messages

view :: Model -> View Msg
view m = 
  div_ 
    []
    [ div_ 
      [ id_ "statusBar" 
      ]
      [ drawStatus m
      ]
    , div_ 
      [ id_ "gameCanvas"
      ]
      [ drawBoard m
      ]
    ]

drawStatus :: Model -> View Msg 
drawStatus Model{..} =
  h2_ 
    [ class_ "gameState" 
    ]
    [ text . toMisoString $ showGameState gameState playerTurn
    ]

drawBoard :: Model -> View Msg
drawBoard Model{..} = 
  div_ 
    [ id_ "board" 
    ]
    $ mapInd drawRow board 
  where
    (rows, cols) = boardSize
    width = 100 `quot` cols
    drawRow row ind = div_ [ class_ "row" ] $ mapInd (drawCol ind) row
    drawCol rowInd Nothing colInd = drawCell rowInd colInd ""
    drawCol rowInd (Just player) colInd = drawCell rowInd colInd (toMisoString $ showPlayer player)
    drawCell row col t = 
      div_ 
        [ class_ "col"
        , style_ $ M.singleton "width" $ toMisoString (show width ++ "%")
        , onClick (Move row col)
        ]
        [ span_ [] [text t]
        ]
        

