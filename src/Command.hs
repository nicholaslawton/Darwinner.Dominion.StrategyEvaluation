module Command
  ( Command(..)
  ) where

import Player
import Card

data Command
  = Noop
  | AddPlayer Player
  | PlayersReady
  | PlaceCardInSupply Card
  | AddCardToDeck PlayerId Card
  deriving (Eq, Show)
