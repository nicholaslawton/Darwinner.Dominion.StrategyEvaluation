module Player
  ( Player(..)
  , dominion
  ) where

import CandidateId
import Card

class Player a where
  playerId :: a -> CandidateId
  deck :: a -> [Card]
  hand :: a -> [Card]
  playedCards :: a -> [Card]
  discard :: a -> [Card]

  alterDeck :: ([Card] -> [Card]) -> a -> a
  alterHand :: ([Card] -> [Card]) -> a -> a
  alterPlayedCards :: ([Card] -> [Card]) -> a -> a
  alterDiscard :: ([Card] -> [Card]) -> a -> a

dominion :: Player p => p -> [Card]
dominion p = concatMap ($ p) [deck, hand, playedCards, discard]
