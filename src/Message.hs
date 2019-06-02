module Message (Message(..)) where

import CandidateId
import Card

data Message 
  = AddPlayer CandidateId 
  | MarkPlayersReady
  | AddCardToDeck CandidateId Card
  | MarkDecksPrepared
  | MarkInitialHandsDrawn
  | PlaceCardInSupply Card
  | MarkSupplyPrepared
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
