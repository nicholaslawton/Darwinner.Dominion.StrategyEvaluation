module PlayerDrawingInitialHand
  ( PlayerDrawingInitialHand
  , PlayerDrawingInitialHand.new
  , fromPlayerWithDeck
  ) where

import GenericPlayer
import Candidate
import Card
import PlayerWithDeck

data PlayerDrawingInitialHand = PlayerDrawingInitialHand
  { playerId' :: CandidateId
  , deck' :: [Card]
  , hand' :: [Card]
  }
  deriving (Eq, Show)

instance GenericPlayer PlayerDrawingInitialHand where
  playerId = playerId'
  deck = deck'
  hand = hand'
  discard = const []

  alterDeck f p = p { deck' = f (deck p) }
  alterHand f p = p { hand' = f (hand p) }
  alterDiscard = const $ error "Cannot alter the discard of a player who is still drawing their initial hand"

new :: CandidateId -> [Card] -> [Card] -> PlayerDrawingInitialHand
new = PlayerDrawingInitialHand

fromPlayerWithDeck :: PlayerWithDeck -> PlayerDrawingInitialHand
fromPlayerWithDeck p = PlayerDrawingInitialHand (playerId p) (deck p) []
