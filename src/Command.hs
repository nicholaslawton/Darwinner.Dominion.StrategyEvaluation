module Command
  ( Command(..)
  ) where

import Candidate
import Card

data Command
  = AddPlayer CandidateId 
  | MarkPlayersReady
  | PlaceCardInSupply Card
  | MarkSupplyPrepared
  | AddCardToDeck CandidateId Card
  | MarkDecksPrepared
  | DrawCard CandidateId Card
  | MarkInitialHandsDrawn
  | GainCard CandidateId Card
  | BuyPhaseComplete
  | EndGame
  deriving (Eq, Show)
