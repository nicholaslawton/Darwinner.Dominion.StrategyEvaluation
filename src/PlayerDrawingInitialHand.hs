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
  deck = PlayerDrawingInitialHand.deck
  hand = PlayerDrawingInitialHand.hand
  discard = const []

fromPlayerPreparingStartingDeck :: PlayerPreparingStartingDeck -> PlayerDrawingInitialHand
fromPlayerPreparingStartingDeck p = PlayerDrawingInitialHand (GenericPlayer.playerId p) (GenericPlayer.deck p) []

alterDeck :: ([Card] -> [Card]) -> PlayerDrawingInitialHand -> PlayerDrawingInitialHand
alterDeck f p = p { PlayerDrawingInitialHand.deck = f (PlayerDrawingInitialHand.deck p) }

alterHand :: ([Card] -> [Card]) -> PlayerDrawingInitialHand -> PlayerDrawingInitialHand
alterHand f p = p { PlayerDrawingInitialHand.hand = f (PlayerDrawingInitialHand.hand p) }
