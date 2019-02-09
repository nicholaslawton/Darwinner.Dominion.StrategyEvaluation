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
import Control.Applicative

import GameStateValidation
import EngineValidation
import ArbitraryInstances()
import CardOrder
import Test.Hspec
import Test.QuickCheck

cleanUpPhaseTests :: SpecWith ()
cleanUpPhaseTests = describe "clean up phase" $ do
  
  it "discards all cards from hand" $ property $ \params (NonEmpty ps) cards ->
    (===) ((sortOn arbitraryCardOrder . hand . head) ps) . sortOn arbitraryCardOrder . mapMaybe cardDiscarded . history . performCleanUpPhase (commandLimit ps) params . gameInCleanUpPhase ps cards

  it "completes" $ property $ \params (NonEmpty ps) cards ->
    (===) EndGame . last . history . performCleanUpPhase (commandLimit ps) params . gameInCleanUpPhase ps cards

  where
    commandLimit = (+10) . length . hand . head

gameInCleanUpPhase :: [CompletePlayer] -> [Card] -> Int -> Game
gameInCleanUpPhase ps cards = Game.mapState (const (CleanUpPhase ps cards)) . Game.new

performCleanUpPhase :: Int -> EvaluationParameters -> Game -> Game
performCleanUpPhase = execUntil . cleanUpPhaseOver

cleanUpPhaseOver :: Int -> Game -> Bool
cleanUpPhaseOver limit = liftA2 (||) (not . inCleanUpPhase . Game.state) ((> limit) . length . Game.history)

cardDiscarded :: Command -> Maybe Card
cardDiscarded (DiscardCard _ card) = Just card
cardDiscarded _ = Nothing
