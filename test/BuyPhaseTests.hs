module BuyPhaseTests (buyPhaseTests) where

import Command
import Game
import GameState
import PlayState
import EvaluationParameters
import CompletePlayer
import Card
import BuyAllowance
import Turn

import Data.Composition

import GameStateValidation
import EngineValidation
import ArbitraryInstances()
import Test.Hspec
import Test.QuickCheck

buyPhaseTests :: SpecWith ()
buyPhaseTests = describe "buy phase" $ do

  it "gains a card" $ property $ \params (Positive buys) (NonEmpty ps) (NonEmpty cards) ->
    any gainCard . history .: runTest params buys ps cards

  it "completes" $ property $ \params (Positive buys) (NonEmpty ps) (NonEmpty cards) ->
    (===) BuyPhaseComplete . last . history .: runTest params buys ps cards

runTest :: EvaluationParameters -> Int -> [CompletePlayer] -> [Card] -> Turn -> Int -> Game
runTest params buys ps cards =
  execWhile buyPhase (commandLimit buys cards) params .: gameInBuyPhase buys ps cards

commandLimit :: Int -> [Card] -> Int
commandLimit buys cards = min buys (length cards) + 10

gameInBuyPhase :: Int -> [CompletePlayer] -> [Card] -> Turn -> Int -> Game
gameInBuyPhase buys = gameInState . BuyPhase (BuyAllowance buys) .:. PlayState

gainCard :: Command -> Bool
gainCard (GainCard _ _) = True
gainCard _ = False
