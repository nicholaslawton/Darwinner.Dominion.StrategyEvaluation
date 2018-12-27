module PlayerPreparingStartingDeck
  ( PlayerPreparingStartingDeck(..)
  , new
  , alterDeck
  ) where

import Candidate
import Card

data PlayerPreparingStartingDeck = PlayerPreparingStartingDeck
  { playerId :: CandidateId
  , deck :: [Card]
  }
  deriving (Eq, Show)

new :: CandidateId -> PlayerPreparingStartingDeck
new pid = PlayerPreparingStartingDeck pid []

alterDeck :: ([Card] -> [Card]) -> PlayerPreparingStartingDeck -> PlayerPreparingStartingDeck
alterDeck f p = p { deck = f (deck p) }
