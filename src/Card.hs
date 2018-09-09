module Card (Card(..)) where

data Card
  = Province
  | Duchy
  | Estate
  | Gold
  | Silver
  | Copper
  deriving (Eq, Show)
