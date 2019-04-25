module Command (Command(..)) where

import Candidate
import Card

data Command
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
