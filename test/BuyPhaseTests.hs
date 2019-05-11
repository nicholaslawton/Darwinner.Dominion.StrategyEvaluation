module BuyPhaseTests (buyPhaseTests) where

import Event
import Game
import GameState
import PlayState
import EvaluationParameters
import Candidate
import Strategy
import CompletePlayer
import Player
import Card
import Coins
import BuyAllowance
import Turn

import Data.Composition
import Data.List
import Data.Maybe

import EventValidation
import GameStateValidation
import EngineValidation
import ArbitraryInstances
import CardOrder
import Test.Hspec
import Test.QuickCheck

buyPhaseTests :: SpecWith ()
buyPhaseTests = describe "buy phase" $ do

  it "plays all treasure cards" $ property $ \(ValidPlayersWithParams ps params) coins (Positive buys) (NonEmpty cards) ->
    (===) (sortOn arbitraryCardOrder . filter ((== Treasure) . cardType) . hand . head $ ps)
      . sortOn arbitraryCardOrder
      . mapMaybe cardPlayed
      . history
      . runTest params coins buys ps cards firstTurn

  it "gains a card which is in both the strategic priority and the supply" $ property $
    \(ValidPlayersWithParams ps params) coins (Positive buys) (NonEmpty cards) ->
      let
        card = head cards
      in
        any cardGained . history . runTest (prioritise card params) (coins + value card) buys ps cards firstTurn

  it "completes" $ property $ \(ValidPlayersWithParams ps params) coins (Positive buys) (NonEmpty cards) ->
    (===) BuyPhaseCompleted . last . history . runTest params coins buys ps cards firstTurn

runTest :: EvaluationParameters -> Coins -> Int -> [CompletePlayer] -> [Card] -> Turn -> Int -> Game
runTest params coins buys ps cards =
  execWhile buyPhase (actionLimit buys ps cards) params .: gameInBuyPhase coins buys ps cards

actionLimit :: Int -> [CompletePlayer] -> [Card] -> Int
actionLimit buys ps cards = min buys (length cards) + length (hand $ head ps) + 10

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
