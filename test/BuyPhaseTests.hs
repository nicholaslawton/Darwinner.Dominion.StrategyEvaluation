module BuyPhaseTests (buyPhaseTests) where

import Event
import Game
import GameState
import PlayState
import EvaluationParameters
import CompletePlayer
import Card
import BuyAllowance
import Turn

import Data.Composition

import EventValidation
import GameStateValidation
import EngineValidation
import ArbitraryInstances
import Test.Hspec
import Test.QuickCheck

buyPhaseTests :: SpecWith ()
buyPhaseTests = describe "buy phase" $ do

  it "gains a card" $ property $ \(ValidPlayersWithParams ps params) (Positive buys) (NonEmpty cards) ->
    any cardGained . history .: runTest params buys ps cards

  it "completes" $ property $ \(ValidPlayersWithParams ps params) (Positive buys) (NonEmpty cards) ->
    (===) BuyPhaseCompleted . last . history .: runTest params buys ps cards

runTest :: EvaluationParameters -> Int -> [CompletePlayer] -> [Card] -> Turn -> Int -> Game
runTest params buys ps cards =
  execWhile buyPhase (actionLimit buys cards) params .: gameInBuyPhase buys ps cards

actionLimit :: Int -> [Card] -> Int
actionLimit buys cards = min buys (length cards) + 10

gameInBuyPhase :: Int -> [CompletePlayer] -> [Card] -> Turn -> Int -> Game
gameInBuyPhase buys = gameInState . BuyPhase (BuyAllowance buys) .:. PlayState
