module Turn
  ( Turn(..)
  , turnsCompleted
  , firstTurn
  ) where

newtype Turn = Turn Int
  deriving (Eq, Show)

turnsCompleted :: Turn -> Int
turnsCompleted (Turn x) = x - 1

firstTurn :: Turn
firstTurn = Turn 1
