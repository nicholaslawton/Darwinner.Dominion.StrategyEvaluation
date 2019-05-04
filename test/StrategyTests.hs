module StrategyTests (strategyTests) where

import Strategy
import PlayState

import ArbitraryInstances()
import Test.Hspec
import Test.QuickCheck

strategyTests :: SpecWith ()
strategyTests = describe "strategy execution" $ do

  it "never picks a card which is not available in the supply" $ property $ \strategy g ->
    maybe True (`elem` supply g) $ execute strategy g

  it "never picks a card which is not in the strategic priority" $ property $ \strategy@(Strategy priority) ->
    maybe True (`elem` priority) . execute strategy
