module Game
  ( Game(state, gen)
  , GameState(..)
  , recordCommand
  , history
  , new
  , players
  ) where

import Card
import Command
import Player

import System.Random

data Game = Game
  { state :: GameState
  , commands :: [Command]
  , gen :: StdGen
  }

data GameState
  = New [Player]
  | PreparingSupply [Player] [Card]
  | Prepared
  deriving (Eq, Show)

recordCommand :: Command -> Game -> Game
recordCommand event game = game { commands = event : commands game }

history :: Game -> [Command]
history = reverse . commands

new :: Int -> Game
new = Game (New []) [] . mkStdGen

players :: Game -> [Player]
players = getPlayers . state
  where
    getPlayers (New ps) = ps
    getPlayers _ = []
