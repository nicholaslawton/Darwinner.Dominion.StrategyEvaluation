module Command
  ( Command(..)
  ) where

import Candidate
import Card

data Command
  = Noop
  | AddPlayer CandidateId 
  | MarkPlayersReady
  | PlaceCardInSupply Card
  | MarkSupplyPrepared
  | AddCardToDeck CandidateId Card
  | MarkDecksPrepared
  | DrawCard CandidateId Card
  deriving (Eq, Show)
