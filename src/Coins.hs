{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Coins
  ( Coins(..)
  , base
  ) where

newtype Coins = Coins Int
  deriving (Eq, Show, Ord, Num)

base :: Coins
base = Coins 0
