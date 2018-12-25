module Command
  ( Command(..)
  ) where

import Candidate
import Card

data Command
  = Noop
  | AddPlayer CandidateId 
  | PlayersReady
  | PlaceCardInSupply Card
  | SupplyReady
  | AddCardToDeck CandidateId Card
  | DecksReady
  | DrawCard CandidateId Card
  deriving (Eq, Show)
