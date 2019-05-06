module Card
  ( Card(..)
  , cost
  ) where

import Coins

data Card
  = Copper
  | Silver
  | Gold
  | Estate
  | Duchy
  | Province
  | Curse
  deriving (Eq, Show)

cost :: Card -> Coins
cost Copper = Coins 0
cost Silver = Coins 3
cost Gold = Coins 6
cost Estate = Coins 2
cost Duchy = Coins 5
cost Province = Coins 8
cost Curse = Coins 0
