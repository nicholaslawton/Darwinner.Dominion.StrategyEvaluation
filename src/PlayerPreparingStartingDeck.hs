module PlayerPreparingStartingDeck
  ( PlayerPreparingStartingDeck(..)
  , new
  , alterDeck
  ) where

import GenericPlayer
import PlayerWithoutDominion
import Candidate
import Card

data PlayerPreparingStartingDeck = PlayerPreparingStartingDeck
  { playerId :: CandidateId
  , deck :: [Card]
  }
  deriving (Eq, Show)

instance GenericPlayer PlayerPreparingStartingDeck where
  playerId = PlayerPreparingStartingDeck.playerId

new :: PlayerWithoutDominion -> PlayerPreparingStartingDeck
new p = PlayerPreparingStartingDeck (GenericPlayer.playerId p) []

alterDeck :: ([Card] -> [Card]) -> PlayerPreparingStartingDeck -> PlayerPreparingStartingDeck
alterDeck f p = p { deck = f (deck p) }
