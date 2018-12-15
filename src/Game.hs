module Game
  ( Game(..)
  , GameState(..)
  , new
  , players
  ) where

import Event
import Player

import System.Random

data Game = Game
  { state :: GameState
  , history :: [Event]
  , gen :: StdGen
  }

data GameState
  = New [Player]
  | Prepared
  deriving (Eq, Show)

new :: Int -> Game
new = Game (New []) [] . mkStdGen

players :: Game -> [Player]
players = getPlayers . state
  where
    getPlayers (New ps) = ps
    getPlayers _ = []
