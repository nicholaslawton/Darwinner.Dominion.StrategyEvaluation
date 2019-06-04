module StrategyTests (strategyTests) where

import Strategy
import Card
import Supply
import PlayState

import Data.Composition

import ArbitraryInstances()
import Test.Hspec
import Test.QuickCheck

strategyTests :: SpecWith ()
strategyTests = describe "strategy execution" $ do

  it "never picks a card which is not available in the supply" $ property $ \strategy coins g ->
    maybe True (`contains` supply g) $ execute strategy coins g

  it "never picks a card which is not in the strategic priority" $ property $ \strategy@(Strategy priority) ->
    maybe True (`elem` priority) .: execute strategy

  it "never picks a card with a cost which exceeds the limit" $ property $ \strategy coins ->
    maybe True ((<= coins) . cost) . execute strategy coins
