module CleanUpPhaseTests (cleanUpPhaseTests) where

import Command
import Game
import GameState
import PlayState
import EvaluationParameters
import Player
import CompletePlayer
import Card
import Turn

import Data.List
import Data.Maybe
import Data.Composition

import GameStateValidation
import EngineValidation
import CommandValidation
import PlayerValidation
import ArbitraryInstances()
import CardOrder
import Test.Hspec
import Test.QuickCheck hiding (Discard)

cleanUpPhaseTests :: SpecWith ()
cleanUpPhaseTests = describe "clean up phase" $ do
  
  it "discards all cards from hand" $ property $ \params (NonEmpty ps) cards ->
    (===) (sortOn arbitraryCardOrder . hand . head $ ps)
      . sortOn arbitraryCardOrder
      . mapMaybe cardDiscarded
      . history
      . runTest params Discard ps cards firstTurn

  it "draws new hand" $ property $ \params (NonEmpty ps) cards ->
    (===) (min 5 . length . dominion $ head ps)
      . length
      . filter ((/=) Nothing . cardDrawn)
      . history
      . runTest params Discard ps cards firstTurn

  it "completes" $ property $ \params (NonEmpty ps) cards ->
    (===) NextHandDrawn . last . history . runTest params Discard ps cards firstTurn

runTest :: EvaluationParameters -> CleanUpStep -> [CompletePlayer] -> [Card] -> Turn -> Int -> Game
runTest params step ps = execWhile cleanUpPhase (commandLimit ps) params .:. gameInCleanUpPhase step ps

commandLimit :: [CompletePlayer] -> Int
commandLimit = (+10) . length . hand . head

gameInCleanUpPhase :: CleanUpStep -> [CompletePlayer] -> [Card] -> Turn -> Int -> Game
gameInCleanUpPhase step = gameInState . CleanUpPhase step .:. PlayState
