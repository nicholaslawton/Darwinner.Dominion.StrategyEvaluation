module ScoringTests (scoringTests) where

import Scoring
import Player
import CompletePlayer
import Card

import ArbitraryInstances
import Test.Hspec
import Test.QuickCheck

scoringTests :: SpecWith ()
scoringTests = describe "scoring" $ do

  it "increases after adding a victory card" $ property $ \player (VictoryCard card) ->
    score (player :: CompletePlayer) <= score (alterDeck (card :) player)

  it "does not change after adding a non-victory card" $ property $ \player (NonVictoryCard card) ->
    score (player :: CompletePlayer) == score (alterDeck (card :) player)

  it "increases relative to cost of victory card" $ property $ \player ->
    addAndScore Province player > addAndScore Duchy player
      && addAndScore Duchy player > addAndScore Estate player
        where
          addAndScore :: Card -> CompletePlayer -> Int
          addAndScore card p = score (alterDeck (card :) p)
