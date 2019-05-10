module CompletePlayer
  ( CompletePlayer
  , CompletePlayer.new
  , CompletePlayer.fromPlayerWithoutDominion
  , CompletePlayer.fromPlayerWithDeck
  , fromPlayerWithHand
  ) where

import Player
import CandidateId
import Card
import PlayerWithoutDominion
import PlayerWithDeck
import PlayerWithHand

data CompletePlayer = CompletePlayer
  { playerId' :: CandidateId
  , deck' :: [Card]
  , hand' :: [Card]
  , playedCards' :: [Card]
  , discard' :: [Card]
  }
  deriving (Eq, Show)

instance Player CompletePlayer where
  playerId = playerId'
  deck = deck'
  hand = hand'
  playedCards = playedCards'
  discard = discard'

  alterDeck f p = p { deck' = f (deck p) }
  alterHand f p = p { hand' = f (hand p) }
  alterPlayedCards f p = p { playedCards' = f (playedCards p) }
  alterDiscard f p = p { discard' = f (discard p) }

new :: CandidateId -> [Card] -> [Card] -> [Card] -> [Card] -> CompletePlayer
new = CompletePlayer 

fromPlayerWithoutDominion :: PlayerWithoutDominion -> CompletePlayer
fromPlayerWithoutDominion p = CompletePlayer (playerId p) [] [] [] []

fromPlayerWithDeck :: PlayerWithDeck -> CompletePlayer
fromPlayerWithDeck p = CompletePlayer (playerId p) (deck p) [] [] []

fromPlayerWithHand :: PlayerWithHand -> CompletePlayer
fromPlayerWithHand p = CompletePlayer (playerId p) (deck p) (hand p) [] []
