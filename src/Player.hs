module Player
  ( Player(..)
  , Player.new
  , Player.fromPlayerPreparingStartingDeck
  , fromPlayerDrawingInitialHand
  , Player.alterDeck
  , Player.alterHand
  ) where

import Candidate
import Card
import PlayerPreparingStartingDeck
import PlayerDrawingInitialHand

data Player = Player
  { playerId :: CandidateId
  , deck :: [Card]
  , hand :: [Card]
  }
  deriving (Eq, Show)

new :: CandidateId -> Player
new pid = Player pid [] []

fromPlayerPreparingStartingDeck :: PlayerPreparingStartingDeck -> Player
fromPlayerPreparingStartingDeck (PlayerPreparingStartingDeck pid d) = Player pid d []

fromPlayerDrawingInitialHand :: PlayerDrawingInitialHand -> Player
fromPlayerDrawingInitialHand (PlayerDrawingInitialHand pid d h) = Player pid d h

alterDeck :: ([Card] -> [Card]) -> Player -> Player
alterDeck f p = p { Player.deck = f (Player.deck p) }

alterHand :: ([Card] -> [Card]) -> Player -> Player
alterHand f p = p { Player.hand = f (Player.hand p) }
