module Strategy
  ( Strategy(..)
  , execute
  ) where

import Card
import PlayState

import Data.List

data Strategy = Strategy [Card]
  deriving (Eq, Show)

execute :: Strategy -> PlayState -> Maybe Card
execute (Strategy priority) g = find (`elem` supply g) priority
