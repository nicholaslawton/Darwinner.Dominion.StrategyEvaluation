module Coins (Coins(..)) where

newtype Coins = Coins Int
  deriving (Eq, Show, Ord)
