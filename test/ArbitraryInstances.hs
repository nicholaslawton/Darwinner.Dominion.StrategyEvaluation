{-# OPTIONS_GHC -fno-warn-orphans #-}

module ArbitraryInstances where

import Control.Applicative
import Test.QuickCheck

import Card
import Player
import Strategy

instance Arbitrary Card where
  arbitrary = oneof [return Province, return Duchy, return Estate, return Gold, return Silver, return Copper]

instance Arbitrary Player where
  arbitrary = liftA2 Player arbitrary arbitrary

instance Arbitrary PlayerId where
  arbitrary = PlayerId <$> arbitrary

instance Arbitrary Strategy where
  arbitrary = Strategy <$> arbitrary
