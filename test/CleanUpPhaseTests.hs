module CleanUpPhaseTests (cleanUpPhaseTests) where

import Command
import Game
import GameState
import PlayState
import EvaluationParameters
import Player
import CompletePlayer
import Card

import Data.List
import Data.Maybe

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
      . runTest params Discard ps cards

  it "draws new hand" $ property $ \params (NonEmpty ps) cards ->
    (===) (min 5 . length . dominion $ head ps)
      . length
      . filter ((/=) Nothing . cardDrawn)
      . history
      . runTest params Discard ps cards

  it "completes" $ property $ \params (NonEmpty ps) cards ->
    (===) EndGame . last . history . runTest params Discard ps cards

runTest :: EvaluationParameters -> CleanUpStep -> [CompletePlayer] -> [Card] -> Int -> Game
runTest params step ps cards = execWhile cleanUpPhase (commandLimit ps) params . gameInCleanUpPhase step ps cards

commandLimit :: [CompletePlayer] -> Int
commandLimit = (+10) . length . hand . head

gameInCleanUpPhase :: CleanUpStep -> [CompletePlayer] -> [Card] -> Int -> Game
gameInCleanUpPhase step ps cards = gameInState $ CleanUpPhase step (PlayState ps cards)

cardDiscarded :: Command -> Maybe Card
cardDiscarded (DiscardCard _ card) = Just card
cardDiscarded _ = Nothing
