module PlayerWithHand
  ( PlayerWithHand
  , PlayerWithHand.new
  , fromPlayerWithDeck
  ) where

import GenericPlayer
import Candidate
import Card
import PlayerWithDeck

data PlayerWithHand = PlayerWithHand
  { playerId' :: CandidateId
  , deck' :: [Card]
  , hand' :: [Card]
  }
  deriving (Eq, Show)

instance GenericPlayer PlayerWithHand where
  playerId = playerId'
  deck = deck'
  hand = hand'
  discard = const []

  alterDeck f p = p { deck' = f (deck p) }
  alterHand f p = p { hand' = f (hand p) }
  alterDiscard = const $ error "Cannot alter the discard of a player who is still drawing their initial hand"

new :: CandidateId -> [Card] -> [Card] -> PlayerWithHand
new = PlayerWithHand

fromPlayerWithDeck :: PlayerWithDeck -> PlayerWithHand
fromPlayerWithDeck p = PlayerWithHand (playerId p) (deck p) []
