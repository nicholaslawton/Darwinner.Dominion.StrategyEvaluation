module Strategy
  ( Strategy(..)
  , execute
  ) where

import Card
import Supply
import Coins
import PlayState

import Control.Applicative
import Data.List

data Strategy = Strategy [Card]
  deriving (Eq, Show)

execute :: Strategy -> Coins -> PlayState -> Maybe Card
execute (Strategy priority) coins g = find (liftA2 (&&) (`contains` supply g) ((<= coins) . cost)) priority
