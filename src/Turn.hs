module Turn
  ( Turn(..)
  , turnsCompleted
  , firstTurn
  , nextTurn
  ) where

newtype Turn = Turn Int
  deriving (Eq, Show)

turnsCompleted :: Turn -> Int
turnsCompleted (Turn x) = x - 1

firstTurn :: Turn
firstTurn = Turn 1

nextTurn :: Turn -> Turn
nextTurn (Turn x) = Turn (x + 1)
