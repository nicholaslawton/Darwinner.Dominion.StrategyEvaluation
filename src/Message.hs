module Message (Message(..)) where

import Candidate
import Card

data Message 
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
  | EndTurn
  | EndGame
  deriving (Eq, Show)
