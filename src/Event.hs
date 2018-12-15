module Event
  ( Event(..)
  ) where

import Player

data Event
  = Noop
  | AddPlayer Player
