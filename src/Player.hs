module Player
  ( Player(..)
  , Player.new
  , fromPlayerPreparingStartingDeck
  , Player.alterDeck
  , alterHand
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

alterDeck :: ([Card] -> [Card]) -> Player -> Player
alterDeck f p = p { Player.deck = f (Player.deck p) }

alterHand :: ([Card] -> [Card]) -> Player -> Player
alterHand f p = p { hand = f (hand p) }
