module Main where

import TicTacToe (startGame, newGame)

main :: IO ()
main = startGame (newGame (3, 3) 3)
