module Player
  ( Player(..)
  , new
  , mapDeck
  , mapHand
  ) where

import Candidate
import Card

data Player = Player
  { playerId :: CandidateId
  , deck :: [Card]
  , hand :: [Card]
  }
  deriving (Eq, Show)

new :: CandidateId -> Player
new pid = Player pid [] []

mapDeck :: ([Card] -> [Card]) -> Player -> Player
mapDeck f p = p { deck = f (deck p) }

mapHand :: ([Card] -> [Card]) -> Player -> Player
mapHand f p = p { hand = f (hand p) }
