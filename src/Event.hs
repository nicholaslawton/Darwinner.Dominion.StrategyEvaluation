module Event
  ( Event(..)
  ) where

import Player
import Card

data Event
  = Noop
  | AddPlayer Player
  | AddCardToSupply Card
  deriving (Eq, Show)
