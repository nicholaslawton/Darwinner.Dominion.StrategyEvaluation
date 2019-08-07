module ScoringTests (scoringTests) where

import Scoring
import Player
import CompletePlayer

import ArbitraryInstances()
import Test.Hspec
import Test.QuickCheck

scoringTests :: SpecWith ()
scoringTests = describe "scoring" $
  it "increases after adding a victory card" $ property $ \player victoryCard ->
    score player <= score (alterDeck (victoryCard :) (player :: CompletePlayer))
