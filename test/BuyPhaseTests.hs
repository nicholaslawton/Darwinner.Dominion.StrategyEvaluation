module BuyPhaseTests where

import Command
import Game
import GameState
import Engine
import EvaluationParameters

import Control.Applicative
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

import ArbitraryInstances()
import Test.Hspec
import Test.QuickCheck

buyPhaseTests :: SpecWith ()
buyPhaseTests = describe "buy phase" $ do

  it "gains a card" $ property $ \seed cards ->
    any gainCard $ history $ performBuyPhase (EvaluationParameters []) (Game.mapState (const (InProgress [] (getNonEmpty cards))) (Game.new seed))

performBuyPhase :: EvaluationParameters -> Game -> Game
performBuyPhase = execUntil buyPhaseOver

execUntil :: (Game -> Bool) -> EvaluationParameters -> Game -> Game
execUntil predicate parameters = execState $ runReaderT (Engine.runUntil predicate) parameters

buyPhaseOver :: Game -> Bool
buyPhaseOver = liftA2 (||) ((== GameOver) . Game.state) ((>10) . length . Game.history)

gainCard :: Command -> Bool
gainCard (GainCard _ _) = True
gainCard _ = False
