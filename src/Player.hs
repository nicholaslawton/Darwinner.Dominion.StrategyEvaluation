module Player
  ( Player(..)
  , PlayerId(..)
  , new
  , mapDeck
  ) where

import Card

data Player = Player
  { playerId :: PlayerId
  , deck :: [Card]
  }
  deriving (Eq, Show)

newtype PlayerId = PlayerId String
  deriving (Eq, Ord, Show)

new :: PlayerId -> Player
new = flip Player []

mapDeck :: ([Card] -> [Card]) -> Player -> Player
mapDeck f p = p { deck = f (deck p) }
