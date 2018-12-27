module Card (Card(..)) where

data Card
  = Copper
  | Silver
  | Gold
  | Estate
  | Duchy
  | Province
  | Curse
  deriving (Eq, Show)
