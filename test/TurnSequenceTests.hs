module TurnSequenceTests (turnSequenceTests) where

import Game
import GameState
import PlayState
import BuyAllowance
import CompletePlayer
import Card

import GameStateValidation
import EngineValidation
import ArbitraryInstances()
import Test.Hspec
import Test.QuickCheck hiding (Discard)

turnSequenceTests :: SpecWith ()
turnSequenceTests = describe "turn sequence" $

  it "does not end immediately" $ property $ \params (NonEmpty ps) cards ->
    not . gameOver . state . execUntil gameOver 1000 params . gameInProgress ps cards

gameInProgress :: [CompletePlayer] -> [Card] -> Int -> Game
gameInProgress ps cards = gameInState $ BuyPhase BuyAllowance.initial (PlayState ps cards)
