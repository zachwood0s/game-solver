{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Games.Types where

import Control.Monad.Writer.Lazy
import Control.Monad.Trans.Reader
import qualified Miso 

import qualified Solvers.Types
import qualified Games.Messages.TicTacToe as TicTacToe
import qualified Games.Messages.Connect4 as Connect4

data Games 
  = TicTacToeGame
  | Connect4Game
  deriving (Show, Eq, Ord)

data Interface action = Interface
  { passAction :: Msg -> action 
  }

data PlayerConfig = PlayerConfig 
  { player1 :: Maybe Solvers.Types.Options
  , player2 :: Maybe Solvers.Types.Options
  }


data Game action = Game 
  { gameType :: Games
  , update :: Interface action -> Msg -> GameM action (Game action)
  , view :: Interface action -> Miso.View action 
  } 

updateGame :: PlayerConfig -> Interface action -> Msg -> Game action -> (Game action, [IO action])
updateGame config iface msg game = runWriter (runReaderT (update game iface msg) config) 

type GameM action = ReaderT PlayerConfig (Writer [IO action]) 

gameAction :: Interface action -> Msg -> GameM action ()
gameAction iface a = gameActions iface [a]

gameActions :: Interface action -> [Msg] -> GameM action ()
gameActions iface actions = gameIO (map (pure . passAction iface ) actions)

gameLog :: Interface action -> IO () -> GameM action () 
gameLog iface a = gameIO [a >> pure (passAction iface NoOp)]

gameIO :: [IO action] -> GameM action ()
gameIO = lift . tell


instance Eq (Game action) where 
  x == y = False -- Making games never equal forces miso to readraw each message, 
                 -- could make this better if it causes problems

{----------------
    Messages
----------------}

data Msg 
  = TicTacToe TicTacToe.Msg
  | Connect4 Connect4.Msg
  | NoOp
  deriving (Show, Eq)

{----------------
    Utils
----------------}

showGame :: Games -> String
showGame TicTacToeGame = "Tic Tac Toe"
showGame Connect4Game = "Connect4"

