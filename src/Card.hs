module Card
  ( Card(..)
  , CardType(..)
  , cardType
  , cost
  , value
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

data CardType
  = Treasure
  | Victory
  | CurseType
  deriving (Eq)

cardType :: Card -> CardType
cardType Copper = Treasure
cardType Silver = Treasure
cardType Gold = Treasure
cardType Estate = Victory
cardType Duchy = Victory
cardType Province = Victory
cardType Curse = CurseType

cost :: Card -> Coins
cost Copper = Coins 0
cost Silver = Coins 3
cost Gold = Coins 6
cost Estate = Coins 2
cost Duchy = Coins 5
cost Province = Coins 8
cost Curse = Coins 0

value :: Card -> Coins
value Copper = Coins 1
value Silver = Coins 2
value Gold = Coins 3
value _ = Coins 0
