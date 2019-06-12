module Supply
  ( Supply(..)
  , empty
  , cards
  , size
  , contains
  , pileSize
  , emptyPiles
  , add
  , remove
  ) where

import Card
import Collections

data Pile = Pile Card Int
  deriving (Eq, Show)

data Supply = Supply [Pile]
  deriving (Eq, Show)

empty :: Supply
empty = Supply []

cards :: Supply -> [Card]
cards = query (>>= \(Pile card count) -> replicate count card)

size :: Supply -> Int
size = query combinedSize

contains :: Card -> Supply -> Bool
contains card = query $ any (\(Pile c count) -> c == card && count > 0)

pileSize :: Card -> Supply -> Int
pileSize card = query $ combinedSize . filter ((== card) . pileOf)

emptyPiles :: Supply -> Int
emptyPiles = query $ length . filter emptyPile

add :: Card -> Supply -> Supply
add card = alter $ alterIf (Pile card 1 :) (all ((/= card) . pileOf)) . alterElem pileOf (alterPileSize (+1)) card

remove :: Card -> Supply -> Supply
remove card = alter $ alterElem pileOf (alterPileSize (subtract 1)) card

query :: ([Pile] -> a) -> Supply -> a
query q (Supply piles) = q piles

alter :: ([Pile] -> [Pile]) -> Supply -> Supply
alter f (Supply piles) = Supply $ f piles

pileOf :: Pile -> Card
pileOf (Pile card _) = card

pileNum :: Pile -> Int
pileNum (Pile _ count) = count

emptyPile :: Pile -> Bool
emptyPile (Pile _ 0) = True
emptyPile _ = False

combinedSize :: [Pile] -> Int
combinedSize = sum . fmap pileNum

alterPileSize :: (Int -> Int) -> Pile -> Pile
alterPileSize f (Pile card count) = Pile card $ max 0 $ f count
