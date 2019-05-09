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
import Coins
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
    \(ValidPlayersWithParams ps params) coins (Positive buys) (NonEmpty cards) ->
      let
        card = head cards
      in
        any cardGained . history .: runTest (prioritise card params) (coins + value card) buys ps cards

  it "completes" $ property $ \(ValidPlayersWithParams ps params) coins (Positive buys) (NonEmpty cards) ->
    (===) BuyPhaseCompleted . last . history .: runTest params coins buys ps cards

runTest :: EvaluationParameters -> Coins -> Int -> [CompletePlayer] -> [Card] -> Turn -> Int -> Game
runTest params coins buys ps cards =
  execWhile buyPhase (actionLimit buys cards) params .: gameInBuyPhase coins buys ps cards

actionLimit :: Int -> [Card] -> Int
actionLimit buys cards = min buys (length cards) + 10

gameInBuyPhase :: Coins -> Int -> [CompletePlayer] -> [Card] -> Turn -> Int -> Game
gameInBuyPhase coins buys = gameInState . BuyPhase coins (BuyAllowance buys) .:. PlayState

prioritise :: Card -> EvaluationParameters -> EvaluationParameters
prioritise card (EvaluationParameters candidates) =
  EvaluationParameters $ alterStrategy (alterPriority (card :)) <$> candidates
    where
      alterStrategy :: (Strategy -> Strategy) -> Candidate -> Candidate
      alterStrategy f candidate = candidate { strategy = f $ strategy candidate }
      alterPriority :: ([Card] -> [Card]) -> Strategy -> Strategy
      alterPriority f (Strategy priority) = Strategy $ f priority
