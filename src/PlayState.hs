module PlayState
  ( PlayState(..)
  , activePlayer
  , gameEndConditions
  ) where

import CompletePlayer
import Card
import Supply
import Turn

data PlayState = PlayState
  { players :: [CompletePlayer]
  , supply :: Supply
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
gameEndConditions = not . contains Province . supply
