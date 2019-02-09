module CleanUpPhaseTests (cleanUpPhaseTests) where

import Command
import Game
import GameState
import EvaluationParameters
import Player
import CompletePlayer
import Card

import Data.List
import Data.Maybe

import GameStateValidation
import EngineValidation
import ArbitraryInstances()
import CardOrder
import Test.Hspec
import Test.QuickCheck

cleanUpPhaseTests :: SpecWith ()
cleanUpPhaseTests = describe "clean up phase" $ do
  
  it "discards all cards from hand" $ property $ \params (NonEmpty ps) cards ->
    (===) (sortOn arbitraryCardOrder . hand . head $ ps)
      . sortOn arbitraryCardOrder
      . mapMaybe cardDiscarded
      . history
      . runTest params ps cards

  it "completes" $ property $ \params (NonEmpty ps) cards ->
    (===) EndGame . last . history . runTest params ps cards

runTest :: EvaluationParameters -> [CompletePlayer] -> [Card] -> Int -> Game
runTest params ps cards = execWhile cleanUpPhase (commandLimit ps) params . gameInCleanUpPhase ps cards

commandLimit :: [CompletePlayer] -> Int
commandLimit = (+10) . length . hand . head

gameInCleanUpPhase :: [CompletePlayer] -> [Card] -> Int -> Game
gameInCleanUpPhase ps cards = gameInState $ CleanUpPhase ps cards

cardDiscarded :: Command -> Maybe Card
cardDiscarded (DiscardCard _ card) = Just card
cardDiscarded _ = Nothing
