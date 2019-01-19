module PlayerPreparingStartingDeck
  ( PlayerPreparingStartingDeck
  , new
  , fromPlayerWithoutDominion
  , alterDeck
  ) where

import GenericPlayer
import PlayerWithoutDominion
import Candidate
import Card

data PlayerPreparingStartingDeck = PlayerPreparingStartingDeck
  { playerId' :: CandidateId
  , deck' :: [Card]
  }
  deriving (Eq, Show)

instance GenericPlayer PlayerPreparingStartingDeck where
  playerId = playerId'
  deck = deck'
  hand = const []
  discard = const []

new :: CandidateId -> [Card] -> PlayerPreparingStartingDeck
new = PlayerPreparingStartingDeck

fromPlayerWithoutDominion :: PlayerWithoutDominion -> PlayerPreparingStartingDeck
fromPlayerWithoutDominion p = PlayerPreparingStartingDeck (playerId p) []

alterDeck :: ([Card] -> [Card]) -> PlayerPreparingStartingDeck -> PlayerPreparingStartingDeck
alterDeck f p = p { deck' = f (deck' p) }
