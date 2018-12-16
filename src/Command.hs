module Command
  ( Event(..)
  ) where

import Player
import Card

data Event
  = Noop
  | AddPlayer Player
  | PlayersReady
  | PlaceCardInSupply Card
  deriving (Eq, Show)
