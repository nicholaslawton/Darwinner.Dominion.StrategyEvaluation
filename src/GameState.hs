module GameState
  ( GameState(..)
  , players
  , supply
  ) where

import Player
import Card

data GameState
  = New [Player]
  | PreparingSupply [Player] [Card]
  | PreparingDecks [Player] [Card]
  | Prepared
  deriving (Eq, Show)

players :: GameState -> [Player]
players (New ps) = ps
players (PreparingSupply ps _) = ps
players (PreparingDecks ps _) = ps
players Prepared = []

supply :: GameState -> [Card]
supply (New _) = []
supply (PreparingSupply _ cards) = cards
supply (PreparingDecks _ cards) = cards
supply Prepared = []
