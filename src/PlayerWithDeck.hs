module PlayerWithDeck
  ( PlayerWithDeck
  , PlayerWithDeck.new
  , fromPlayerWithoutDominion
  ) where

import Player
import PlayerWithoutDominion
import CandidateId
import Card

data PlayerWithDeck = PlayerWithDeck
  { playerId' :: CandidateId
  , deck' :: [Card]
  }
  deriving (Eq, Show)

instance Player PlayerWithDeck where
  playerId = playerId'
  deck = deck'
  hand = const []
  playedCards = const []
  discard = const []

  alterDeck f p = p { deck' = f (deck p) }
  alterHand = const $ error "Cannot alter the hand of a player who does not have one"
  alterPlayedCards = const $ error "Cannot alter the played cards of a player who cannot play any"
  alterDiscard = const $ error "Cannot alter the discard of a player who does not have one"

new :: CandidateId -> [Card] -> PlayerWithDeck
new = PlayerWithDeck

fromPlayerWithoutDominion :: PlayerWithoutDominion -> PlayerWithDeck
fromPlayerWithoutDominion p = PlayerWithDeck (playerId p) []
