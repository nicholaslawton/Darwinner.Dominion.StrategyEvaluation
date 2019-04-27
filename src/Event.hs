module Event (Event(..)) where

import CandidateId
import Card

data Event
  = PlayerAdded CandidateId 
  | PlayersReady
  | CardPlacedInSupply Card
  | SupplyPrepared
  | CardAddedToDeck CandidateId Card
  | DecksPrepared
  | InitialHandsDrawn
  | CardDrawn CandidateId Card
  | CardGained CandidateId Card
  | CardDiscarded CandidateId Card
  | DeckReformed CandidateId
  | BuyPhaseCompleted
  | HandAndPlayedCardsDiscarded
  | NextHandDrawn
  | TurnEnded
  | GameEnded
  deriving (Eq, Show)
