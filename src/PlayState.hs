module PlayState
  ( PlayState(..)
  , activePlayer
  ) where

import CompletePlayer
import Card

import Data.Maybe

data PlayState = PlayState
  { players :: [CompletePlayer]
  , supply :: [Card]
  }
  deriving (Eq, Show)

activePlayer :: PlayState -> CompletePlayer
activePlayer = fromMaybe unexpected . listToMaybe . players
  where
    unexpected = error "Unexpected game in progress with no players"
