module GenericPlayer where

import Candidate
import Card

class GenericPlayer a where
  playerId :: a -> CandidateId
  deck :: a -> [Card]
  hand :: a -> [Card]
  discard :: a -> [Card]

  alterDeck :: ([Card] -> [Card]) -> a -> a
