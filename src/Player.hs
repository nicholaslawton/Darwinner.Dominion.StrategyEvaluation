module Player
  ( Player(..)
  , PlayerId(..)
  , new
  , mapDeck
  ) where

import Strategy
import Card

data Player = Player
  { playerId :: PlayerId
  , strategy :: Strategy
  , deck :: [Card]
  }
  deriving (Eq, Show)

newtype PlayerId = PlayerId String
  deriving (Eq, Ord, Show)

new :: PlayerId -> Strategy -> Player
new pid strat = Player pid strat []

mapDeck :: ([Card] -> [Card]) -> Player -> Player
mapDeck f p = p { deck = f (deck p) }
