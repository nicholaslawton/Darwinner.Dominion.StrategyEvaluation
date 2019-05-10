module PlayerWithHand
  ( PlayerWithHand
  , PlayerWithHand.new
  , fromPlayerWithDeck
  ) where

import Player
import CandidateId
import Card
import PlayerWithDeck

data PlayerWithHand = PlayerWithHand
  { playerId' :: CandidateId
  , deck' :: [Card]
  , hand' :: [Card]
  }
  deriving (Eq, Show)

instance Player PlayerWithHand where
  playerId = playerId'
  deck = deck'
  hand = hand'
  playedCards = const []
  discard = const []

  alterDeck f p = p { deck' = f (deck p) }
  alterHand f p = p { hand' = f (hand p) }
  alterPlayedCards = const $ error "Cannot alter the played cards of a player who is still drawing their initial hand"
  alterDiscard = const $ error "Cannot alter the discard of a player who is still drawing their initial hand"

new :: CandidateId -> [Card] -> [Card] -> PlayerWithHand
new = PlayerWithHand

fromPlayerWithDeck :: PlayerWithDeck -> PlayerWithHand
fromPlayerWithDeck p = PlayerWithHand (playerId p) (deck p) []
