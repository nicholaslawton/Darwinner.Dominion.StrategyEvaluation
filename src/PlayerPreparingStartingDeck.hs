module PlayerPreparingStartingDeck
  ( PlayerPreparingStartingDeck
  , PlayerPreparingStartingDeck.new
  , fromPlayerWithoutDominion
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

  alterDeck f p = p { deck' = f (deck p) }
  alterHand = const $ error "Cannot alter the hand of a player who is still preparing the starting deck"
  alterDiscard = const $ error "Cannot alter the discard of a player who is still preparing the starting deck"

new :: CandidateId -> [Card] -> PlayerPreparingStartingDeck
new = PlayerPreparingStartingDeck

fromPlayerWithoutDominion :: PlayerWithoutDominion -> PlayerPreparingStartingDeck
fromPlayerWithoutDominion p = PlayerPreparingStartingDeck (playerId p) []
