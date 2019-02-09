module CleanUpPhaseTests (cleanUpPhaseTests) where

import Command
import Game
import GameState
import EvaluationParameters
import CompletePlayer
import Card

import Control.Applicative

import GameStateValidation
import EngineValidation
import ArbitraryInstances()
import Test.Hspec
import Test.QuickCheck

cleanUpPhaseTests :: SpecWith ()
cleanUpPhaseTests = describe "clean up phase" $

  it "completes" $ property $ \params ps cards ->
    (===) EndGame . last . history . performCleanUpPhase commandLimit params . gameInCleanUpPhase ps cards
      where
        commandLimit = 1

gameInCleanUpPhase :: [CompletePlayer] -> [Card] -> Int -> Game
gameInCleanUpPhase ps cards = Game.mapState (const (CleanUpPhase ps cards)) . Game.new

performCleanUpPhase :: Int -> EvaluationParameters -> Game -> Game
performCleanUpPhase = execUntil . cleanUpPhaseOver

cleanUpPhaseOver :: Int -> Game -> Bool
cleanUpPhaseOver limit = liftA2 (||) (not . inCleanUpPhase . Game.state) ((> limit) . length . Game.history)
