module Player
  ( Player(..)
  , PlayerId(..)
  ) where

import Strategy

data Player = Player
  { playerId :: PlayerId
  , strategy :: Strategy
  }
  deriving (Eq, Show)

newtype PlayerId = PlayerId String
  deriving (Eq, Ord, Show)
