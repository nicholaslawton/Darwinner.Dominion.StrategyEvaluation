module Player
  ( Player
  , Player.new
  , Player.fromPlayerWithoutDominion
  , Player.fromPlayerWithDeck
  , fromPlayerWithHand
  ) where

import GenericPlayer
import Candidate
import Card
import PlayerWithoutDominion
import PlayerWithDeck
import PlayerWithHand

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

fromPlayerWithHand :: PlayerWithHand -> Player
fromPlayerWithHand p = Player (playerId p) (deck p) (hand p) []
