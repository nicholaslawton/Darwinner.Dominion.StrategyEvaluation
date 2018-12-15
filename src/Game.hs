module Game
  ( Game(state, gen)
  , GameState(..)
  , recordEvent
  , history
  , new
  , players
  ) where

import Card
import Event
import Player

import System.Random

data Game = Game
  { state :: GameState
  , events :: [Event]
  , gen :: StdGen
  }

data GameState
  = New [Player]
  | PreparingSupply [Player] [Card]
  | Prepared
  deriving (Eq, Show)

recordEvent :: Event -> Game -> Game
recordEvent event game = game { events = event : events game }

history :: Game -> [Event]
history = reverse . events

new :: Int -> Game
new = Game (New []) [] . mkStdGen

players :: Game -> [Player]
players = getPlayers . state
  where
    getPlayers (New ps) = ps
    getPlayers _ = []
