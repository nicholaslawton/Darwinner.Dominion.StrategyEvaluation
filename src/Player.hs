module Player
  ( Player(..)
  , new
  , mapDeck
  ) where

import Candidate
import Card

data Player = Player
  { playerId :: CandidateId
  , deck :: [Card]
  }
  deriving (Eq, Show)

new :: CandidateId -> Player
new = flip Player []

mapDeck :: ([Card] -> [Card]) -> Player -> Player
mapDeck f p = p { deck = f (deck p) }
