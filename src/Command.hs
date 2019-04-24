module Command (Command(..)) where

import Candidate
import Card

data Command
  = AddPlayer CandidateId 
  | MarkPlayersReady
  | PlaceCardInSupply Card
  | MarkSupplyPrepared
  | AddCardToDeck CandidateId Card
  | MarkDecksPrepared
  | MarkInitialHandsDrawn
  | DrawCard CandidateId Card
  | GainCard CandidateId Card
  | DiscardCard CandidateId Card
  | ReformDeck CandidateId
  | BuyPhaseComplete
  | DiscardStepComplete
  | DrawHandStepComplete
  | EndGame
  deriving (Eq, Show)
