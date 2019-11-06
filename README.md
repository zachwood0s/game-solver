# Generic Game Solver

## Running the UI

Using nix:

```bash
cd $(nix-build) && bin/server
```

**Note**: The current working directory is important when running the server. The server won't be able to find the clientside part of the app when running the server from some place *other* than the folder with `bin` and `static`. This will prevent the javascript from loading and make the buttons not work. In other news, that's a great way of looking at the part of the app sent by the server.

## Structure

### Games

The game type looks like this:
```haskell
data Game action = Game 
  { gameType :: Games
  , update :: Interface action -> Msg -> GameM action (Game action)
  , view :: Interface action -> Miso.View action 
  } 
```
`GameM` is a Reader/Writer monad which keeps track of the actions a game wants to perform as well as the configuration of the game. The configuration currently consists of the solver options for both player 1 and player 2.

Notice the `Game` type has no mention of a "game state". A typical instantiation of a game would look something like:
```haskell
ticTacToe :: GameState -> Game
ticTacToe m = Game 
  { Games.Types.update = Games.TicTacToe.update m
  , Games.Types.view = Games.TicTacToe.view m
  , Games.Types.gameType = TicTacToeGame
  }
```
Where the specific `update` and `view` functions are partially applied with the "game state".

### Solvers
A solver is just a record of functions that describes how a game should be played and how the solver result should be constructed.
The Solver type looks like this:
```haskell
data Solver a b = Solver 
  { evaluateScore :: a -> Int                       -- Calculate the score for the current game state
  , buildNode :: ABScore Int -> a -> SolverResult b -- Build a solverResult node from a games state
  , getMoves :: a -> [a]                            -- Retrieve all valid possible moves of a game state
  , nextPlayer :: a -> Bool -> Bool                 -- Decide who the next player is
  , showNode :: b -> String                         -- Print the output nodes
  , generateHash :: a -> Int                        -- Hash a the current game state
  }
```
The `generateHash` function is used for key lookups in solvers that can cache previous results.
