module PlayerDrawingInitialHand
  ( PlayerDrawingInitialHand
  , PlayerDrawingInitialHand.new
  , fromPlayerPreparingStartingDeck
  , PlayerDrawingInitialHand.alterDeck
  , alterHand
  ) where

import GenericPlayer
import Candidate
import Card
import PlayerPreparingStartingDeck

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

new :: CandidateId -> [Card] -> [Card] -> PlayerDrawingInitialHand
new = PlayerDrawingInitialHand

fromPlayerPreparingStartingDeck :: PlayerPreparingStartingDeck -> PlayerDrawingInitialHand
fromPlayerPreparingStartingDeck p = PlayerDrawingInitialHand (playerId p) (deck p) []

alterDeck :: ([Card] -> [Card]) -> PlayerDrawingInitialHand -> PlayerDrawingInitialHand
alterDeck f p = p { deck' = f (deck' p) }

alterHand :: ([Card] -> [Card]) -> PlayerDrawingInitialHand -> PlayerDrawingInitialHand
alterHand f p = p { hand' = f (hand' p) }
