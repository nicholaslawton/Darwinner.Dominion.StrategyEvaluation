module Command
  ( Command(..)
  ) where

import Candidate
import Card

data Command
  = Noop
  | AddPlayer PlayerId 
  | PlayersReady
  | PlaceCardInSupply Card
  | SupplyReady
  | AddCardToDeck PlayerId Card
  deriving (Eq, Show)
