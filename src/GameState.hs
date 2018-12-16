module GameState
  ( GameState(..)
  , players
  ) where

import Player
import Card

data GameState
  = New [Player]
  | PreparingSupply [Player] [Card]
  | Prepared
  deriving (Eq, Show)

players :: GameState -> [Player]
players (New ps) = ps
players (PreparingSupply ps _) = ps
players Prepared = []
