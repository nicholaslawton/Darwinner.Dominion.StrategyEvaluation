module PlayerDrawingInitialHand
  ( PlayerDrawingInitialHand(..)
  , fromPlayerPreparingStartingDeck
  , PlayerDrawingInitialHand.alterDeck
  , alterHand
  ) where

import GenericPlayer
import Candidate
import Card
import PlayerPreparingStartingDeck

data PlayerDrawingInitialHand = PlayerDrawingInitialHand
  { playerId :: CandidateId
  , deck :: [Card]
  , hand :: [Card]
  }
  deriving (Eq, Show)

instance GenericPlayer PlayerDrawingInitialHand where
  playerId = PlayerDrawingInitialHand.playerId

fromPlayerPreparingStartingDeck :: PlayerPreparingStartingDeck -> PlayerDrawingInitialHand
fromPlayerPreparingStartingDeck (PlayerPreparingStartingDeck pid d) = PlayerDrawingInitialHand pid d []

alterDeck :: ([Card] -> [Card]) -> PlayerDrawingInitialHand -> PlayerDrawingInitialHand
alterDeck f p = p { PlayerDrawingInitialHand.deck = f (PlayerDrawingInitialHand.deck p) }

alterHand :: ([Card] -> [Card]) -> PlayerDrawingInitialHand -> PlayerDrawingInitialHand
alterHand f p = p { hand = f (hand p) }
