module PlayerValidation (comparableDominion) where

import Player
import Card
import CardOrder

import Data.List

comparableDominion :: Player p => p -> [Card]
comparableDominion = sortOn arbitraryCardOrder . dominion
