module Turn
  ( Turn(..)
  , firstTurn
  ) where

newtype Turn = Turn Int
  deriving (Eq, Show)

firstTurn :: Turn
firstTurn = Turn 1
