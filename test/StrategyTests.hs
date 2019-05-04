module StrategyTests (strategyTests) where

import Strategy
import PlayState

import ArbitraryInstances()
import Test.Hspec
import Test.QuickCheck

strategyTests :: SpecWith ()
strategyTests = describe "strategy execution" $

  it "never picks a card which is not available in the supply" $ property $ \strategy g ->
    maybe True (`elem` (supply g)) (execute strategy g)
