module BuyPhaseTests (buyPhaseTests) where

import Event
import Game
import GameState
import PlayState
import EvaluationParameters
import Candidate
import Strategy
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

  it "gains a card which is in both the strategic priority and the supply" $ property $
    \(ValidPlayersWithParams ps params) (Positive buys) (NonEmpty cards) ->
      any cardGained . history .: runTest (prioritise (head cards) params) buys ps cards

  it "completes" $ property $ \(ValidPlayersWithParams ps params) (Positive buys) (NonEmpty cards) ->
    (===) BuyPhaseCompleted . last . history .: runTest params buys ps cards

runTest :: EvaluationParameters -> Int -> [CompletePlayer] -> [Card] -> Turn -> Int -> Game
runTest params buys ps cards =
  execWhile buyPhase (actionLimit buys cards) params .: gameInBuyPhase buys ps cards

actionLimit :: Int -> [Card] -> Int
actionLimit buys cards = min buys (length cards) + 10

gameInBuyPhase :: Int -> [CompletePlayer] -> [Card] -> Turn -> Int -> Game
gameInBuyPhase buys = gameInState . BuyPhase (BuyAllowance buys) .:. PlayState

prioritise :: Card -> EvaluationParameters -> EvaluationParameters
prioritise card (EvaluationParameters candidates) =
  EvaluationParameters $ alterStrategy (alterPriority (card :)) <$> candidates
    where
      alterStrategy :: (Strategy -> Strategy) -> Candidate -> Candidate
      alterStrategy f candidate = candidate { strategy = f $ strategy candidate }
      alterPriority :: ([Card] -> [Card]) -> Strategy -> Strategy
      alterPriority f (Strategy priority) = Strategy $ f priority
