module Strategy
  ( Strategy(..)
  , execute
  ) where

import Card
import PlayState

import Data.Maybe

data Strategy = Strategy [Card]
  deriving (Eq, Show)

execute :: Strategy -> PlayState -> Maybe Card
execute _ = listToMaybe . supply
