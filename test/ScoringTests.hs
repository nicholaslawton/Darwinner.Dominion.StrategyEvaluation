module ScoringTests (scoringTests) where

import Scoring
import Player
import CompletePlayer

import ArbitraryInstances
import Test.Hspec
import Test.QuickCheck

scoringTests :: SpecWith ()
scoringTests = describe "scoring" $ do

  it "increases after adding a victory card" $ property $ \player (VictoryCard card) ->
    score player <= score (alterDeck (card :) (player :: CompletePlayer))

  it "does not change after adding a non-victory card" $ property $ \player (NonVictoryCard card) ->
    score player == score (alterDeck (card :) (player :: CompletePlayer))
