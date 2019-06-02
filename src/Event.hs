module Event (Event(..)) where

import CandidateId
import Card

data Event
  = PlayerAdded CandidateId 
  | PlayersReady
  | CardAddedToDeck CandidateId Card
  | DecksPrepared
  | InitialHandsDrawn
  | CardPlacedInSupply Card
  | SupplyPrepared
  | CardDrawn CandidateId Card
  | CardGained CandidateId Card
  | TreasureCardPlayed CandidateId Card
  | UnplayedCardDiscarded CandidateId Card
  | PlayedCardDiscarded CandidateId Card
  | DeckReformed CandidateId
  | BuyPhaseCompleted
  | HandAndPlayedCardsDiscarded
  | NextHandDrawn
  | TurnEnded
  | GameEnded
  deriving (Eq, Show)
