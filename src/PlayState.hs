module PlayState (PlayState(..)) where

import CompletePlayer
import Card

data PlayState = PlayState
  { players :: [CompletePlayer]
  , supply :: [Card]
  }
  deriving (Eq, Show)
