module Message (Message(..)) where

import CandidateId
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
  | PlayTreasureCard CandidateId Card
  | DiscardUnplayedCard CandidateId Card
  | DiscardPlayedCard CandidateId Card
  | ReformDeck CandidateId
  | BuyPhaseComplete
  | DiscardStepComplete
  | DrawHandStepComplete
  | EndTurn
  | EndGame
  deriving (Eq, Show)
