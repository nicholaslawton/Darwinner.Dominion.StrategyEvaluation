module Player
  ( Player
  , Player.new
  , Player.fromPlayerWithoutDominion
  , Player.fromPlayerPreparingStartingDeck
  , fromPlayerDrawingInitialHand
  , Player.alterHand
  , alterDiscard
  ) where

import GenericPlayer
import Candidate
import Card
import PlayerWithoutDominion
import PlayerPreparingStartingDeck
import PlayerDrawingInitialHand

data Player = Player
  { playerId' :: CandidateId
  , deck' :: [Card]
  , hand' :: [Card]
  , discard' :: [Card]
  }
  deriving (Eq, Show)

instance GenericPlayer Player where
  playerId = playerId'
  deck = deck'
  hand = hand'
  discard = discard'

  alterDeck f p = p { deck' = f (deck p) }

new :: CandidateId -> [Card] -> [Card] -> [Card] -> Player
new = Player 

fromPlayerWithoutDominion :: PlayerWithoutDominion -> Player
fromPlayerWithoutDominion p = Player (playerId p) [] [] []

fromPlayerPreparingStartingDeck :: PlayerPreparingStartingDeck -> Player
fromPlayerPreparingStartingDeck p = Player (playerId p) (deck p) [] []

fromPlayerDrawingInitialHand :: PlayerDrawingInitialHand -> Player
fromPlayerDrawingInitialHand p = Player (playerId p) (deck p) (hand p) []

alterHand :: ([Card] -> [Card]) -> Player -> Player
alterHand f p = p { hand' = f (hand' p) }

alterDiscard :: ([Card] -> [Card]) -> Player -> Player
alterDiscard f p = p { discard' = f (discard' p) }
