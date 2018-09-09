module Strategy
  (Strategy(..)) where

import Card

data Strategy = Strategy [Card]
  deriving (Eq, Show)
