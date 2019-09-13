module Main where

import ExtendedNum (ExtendedNum(..))
import TicTacToe (startGame, newGame, minimaxAB, buildTree, Player(..))

main :: IO ()
main = startGame (newGame (6, 5) 4)
{-main = do
  let x = minimaxAB X $ buildTree (-1) $ newGame (3, 3) 3
  print "Result"
  print x
  -}

