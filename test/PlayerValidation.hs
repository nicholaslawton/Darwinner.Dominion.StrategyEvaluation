module PlayerValidation (dominion) where

import Player
import Card
import CardOrder

import Data.List

dominion :: Player p => p -> [Card]
dominion p = sortOn arbitraryCardOrder $ concatMap ($ p) [deck, hand, playedCards, discard]
