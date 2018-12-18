module Game
  ( Game(state, gen)
  , recordCommand
  , mapState
  , history
  , Game.new
  , Game.players
  ) where

import GameState
import Command
import Player

import System.Random

data Game = Game
  { state :: GameState
  , commands :: [Command]
  , gen :: StdGen
  }

mapState :: (GameState -> GameState) -> Game -> Game
mapState f game = game { Game.state = f (Game.state game) }

recordCommand :: Command -> Game -> Game
recordCommand event game = game { commands = event : commands game }

history :: Game -> [Command]
history = reverse . commands

new :: Int -> Game
new = Game (New []) [] . mkStdGen

players :: Game -> [Player]
players = GameState.players . state
