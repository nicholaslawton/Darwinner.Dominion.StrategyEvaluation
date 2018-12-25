module Player
  ( Player(..)
  , new
  , mapDeck
  ) where

import Candidate
import Card

data Player = Player
  { playerId :: PlayerId
  , deck :: [Card]
  }
  deriving (Eq, Show)

new :: PlayerId -> Player
new = flip Player []

mapDeck :: ([Card] -> [Card]) -> Player -> Player
mapDeck f p = p { deck = f (deck p) }
