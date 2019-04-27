module PlayerWithoutDominion
  ( PlayerWithoutDominion
  , new
  ) where

import Player
import CandidateId

data PlayerWithoutDominion = PlayerWithoutDominion CandidateId
  deriving (Eq, Show)

instance Player PlayerWithoutDominion where
  playerId (PlayerWithoutDominion pid) = pid
  deck = const []
  hand = const []
  discard = const []

  alterDeck = const $ error "Cannot alter the deck of a player who has no dominion"
  alterHand = const $ error "Cannot alter the hand of a player who has no dominion"
  alterDiscard = const $ error "Cannot alter the discard of a player who has no dominion"

new :: CandidateId -> PlayerWithoutDominion
new = PlayerWithoutDominion
