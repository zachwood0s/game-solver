module Main where

import TicTacToe (startGame, newGame)

main :: IO ()
main = startGame (newGame (6, 6) 3)
