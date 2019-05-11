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

  it "plays all treasure cards" $ property $
    \(ValidPlayersWithParams ps params) coins (Positive buys) (NonEmpty cards) t ->
      let
        g = PlayState ps cards t
      in
        (===) (sortOn arbitraryCardOrder . filter ((== Treasure) . cardType) . hand $ activePlayer g)
          . sortOn arbitraryCardOrder
          . mapMaybe cardPlayed
          . history
          . runTest params coins buys g

  it "gains a card which is in both the strategic priority and the supply" $ property $
    \(ValidPlayersWithParams ps params) coins (Positive buys) (NonEmpty cards) t ->
      let
        g = PlayState ps cards t
        card = head cards
      in
        any cardGained . history . runTest (prioritise card params) (coins + value card) buys g

  it "completes" $ property $ \(ValidPlayersWithParams ps params) coins (Positive buys) (NonEmpty cards) t ->
    let
      g = PlayState ps cards t
    in
      (===) BuyPhaseCompleted . last . history . runTest params coins buys g

runTest :: EvaluationParameters -> Coins -> Int -> PlayState -> Int -> Game
runTest params coins buys g@(PlayState ps cards _) =
  execWhile buyPhase (actionLimit buys ps cards) params . gameInBuyPhase coins buys g

actionLimit :: Int -> [CompletePlayer] -> [Card] -> Int
actionLimit buys ps cards = min buys (length cards) + sum (length . hand <$> ps) + 10

gameInBuyPhase :: Coins -> Int -> PlayState -> Int -> Game
gameInBuyPhase coins buys g = gameInState $ BuyPhase coins (BuyAllowance buys) g

prioritise :: Card -> EvaluationParameters -> EvaluationParameters
prioritise card (EvaluationParameters candidates) =
  EvaluationParameters $ alterStrategy (alterPriority (card :)) <$> candidates
    where
      alterStrategy :: (Strategy -> Strategy) -> Candidate -> Candidate
      alterStrategy f candidate = candidate { strategy = f $ strategy candidate }
      alterPriority :: ([Card] -> [Card]) -> Strategy -> Strategy
      alterPriority f (Strategy priority) = Strategy $ f priority
