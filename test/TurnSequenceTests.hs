module TurnSequenceTests (turnSequenceTests) where

import Game
import GameState
import PlayState
import BuyAllowance
import CompletePlayer
import Card
import Turn

import Data.Composition

import GameStateValidation
import EngineValidation
import ArbitraryInstances()
import Test.Hspec
import Test.QuickCheck hiding (Discard)

turnSequenceTests :: SpecWith ()
turnSequenceTests = describe "turn sequence" $

  it "does not end immediately" $ property $ \params (NonEmpty ps) ->
    not . gameOver . state . execUntil gameOver 1000 params .:. gameInProgress ps

gameInProgress :: [CompletePlayer] -> [Card] -> Turn -> Int -> Game
gameInProgress = gameInState . BuyPhase BuyAllowance.initial .:. PlayState
