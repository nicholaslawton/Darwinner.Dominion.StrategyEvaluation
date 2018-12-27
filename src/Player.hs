module Player
  ( Player(..)
  , Player.new
  , fromPlayerPreparingStartingDeck
  , mapDeck
  , mapHand
  ) where

import Candidate
import Card
import PlayerPreparingStartingDeck

data Player = Player
  { playerId :: CandidateId
  , deck :: [Card]
  , hand :: [Card]
  }
  deriving (Eq, Show)

new :: CandidateId -> Player
new pid = Player pid [] []

fromPlayerPreparingStartingDeck :: PlayerPreparingStartingDeck -> Player
fromPlayerPreparingStartingDeck (PlayerPreparingStartingDeck pid d) = Player pid d []

mapDeck :: ([Card] -> [Card]) -> Player -> Player
mapDeck f p = p { Player.deck = f (Player.deck p) }

mapHand :: ([Card] -> [Card]) -> Player -> Player
mapHand f p = p { hand = f (hand p) }
