module Main where

import ExtendedNum (ExtendedNum(..))
import TicTacToe (startGame, newGame, minimaxAB, buildTree, Player(..))

main :: IO ()
--main = startGame (newGame (4, 4) 3)
main = do
  let x = minimaxAB X NegInf PosInf $ buildTree 2 $ newGame (3, 3) 3
  print x

