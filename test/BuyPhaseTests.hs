module BuyPhaseTests (buyPhaseTests) where

import Command
import Game
import GameState
import EvaluationParameters
import CompletePlayer
import Card
import BuyAllowance

import Control.Applicative

import GameStateValidation
import EngineValidation
import ArbitraryInstances()
import Test.Hspec
import Test.QuickCheck

buyPhaseTests :: SpecWith ()
buyPhaseTests = describe "buy phase" $ do

  it "gains a card" $ property $ \params (Positive buys) (NonEmpty ps) (NonEmpty cards) ->
    any gainCard . history . performBuyPhase 10 params . gameInBuyPhase buys ps cards

  it "completes" $ property $ \params (Positive buys) (NonEmpty ps) (NonEmpty cards) ->
    (===) BuyPhaseComplete . last . history . performBuyPhase (min buys (length cards) + 10) params . gameInBuyPhase buys ps cards

gameInBuyPhase :: Int -> [CompletePlayer] -> [Card] -> Int -> Game
gameInBuyPhase buys ps cards = Game.mapState (const (BuyPhase (BuyAllowance buys) ps cards)) . Game.new

performBuyPhase :: Int -> EvaluationParameters -> Game -> Game
performBuyPhase = execUntil . buyPhaseOver

buyPhaseOver :: Int -> Game -> Bool
buyPhaseOver limit = liftA2 (||) (not . inBuyPhase . Game.state) ((> limit) . length . Game.history)

gainCard :: Command -> Bool
gainCard (GainCard _ _) = True
gainCard _ = False
