module PlayState
  ( PlayState(..)
  , activePlayer
  , gameEndConditions
  ) where

import CompletePlayer
import Card
import Turn

data PlayState = PlayState
  { players :: [CompletePlayer]
  , supply :: [Card]
  , turn :: Turn
  }
  deriving (Eq, Show)

activePlayer :: PlayState -> CompletePlayer
activePlayer g =
  let
    ps = players g
    activeIndex = turnsCompleted (turn g) `mod` length ps
  in
    if null ps
    then error "Unexpected game in progress with no players"
    else ps !! activeIndex

gameEndConditions :: PlayState -> Bool
gameEndConditions = notElem Province . supply
