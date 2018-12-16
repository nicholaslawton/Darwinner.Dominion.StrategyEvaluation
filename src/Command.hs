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
  deriving (Eq, Show)
