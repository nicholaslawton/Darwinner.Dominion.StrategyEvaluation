module Command
  ( Command(..)
  ) where

import Player
import Card

data Command
  = Noop
  | AddPlayer PlayerId 
  | PlayersReady
  | PlaceCardInSupply Card
  | SupplyReady
  | AddCardToDeck PlayerId Card
  deriving (Eq, Show)
