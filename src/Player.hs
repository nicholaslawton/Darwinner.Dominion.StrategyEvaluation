module Player
  ( Player
  , Player.new
  , Player.fromPlayerWithoutDominion
  , Player.fromPlayerWithDeck
  , fromPlayerDrawingInitialHand
  ) where

import GenericPlayer
import Candidate
import Card
import PlayerWithoutDominion
import PlayerWithDeck
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
  alterHand f p = p { hand' = f (hand p) }
  alterDiscard f p = p { discard' = f (discard p) }

new :: CandidateId -> [Card] -> [Card] -> [Card] -> Player
new = Player 

fromPlayerWithoutDominion :: PlayerWithoutDominion -> Player
fromPlayerWithoutDominion p = Player (playerId p) [] [] []

fromPlayerWithDeck :: PlayerWithDeck -> Player
fromPlayerWithDeck p = Player (playerId p) (deck p) [] []

fromPlayerDrawingInitialHand :: PlayerDrawingInitialHand -> Player
fromPlayerDrawingInitialHand p = Player (playerId p) (deck p) (hand p) []
