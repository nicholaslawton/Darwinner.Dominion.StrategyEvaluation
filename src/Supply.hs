module Supply
  ( Supply(..)
  , empty
  , cards
  , size
  , contains
  , pileSize
  , add
  , remove
  ) where

import Card

import Data.List
import Data.List.Unique

data Supply = Supply [Card]
  deriving (Eq, Show)

empty :: Supply
empty = Supply []

query :: ([Card] -> a) -> Supply -> a
query q (Supply s) = q s

cards :: Supply -> [Card]
cards = query id

size :: Supply -> Int
size = query length

contains :: Card -> Supply -> Bool
contains card = query (elem card)

pileSize :: Card -> Supply -> Int
pileSize card = query (countElem card)

alter :: ([Card] -> [Card]) -> Supply -> Supply
alter f (Supply s) = Supply $ f s

add :: Card -> Supply -> Supply
add card = alter (card :)

remove :: Card -> Supply -> Supply
remove card = alter (delete card)
