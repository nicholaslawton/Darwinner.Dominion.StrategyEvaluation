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

  it "gains a card" $ property $ \seed (Positive buys) (NonEmpty ps) (NonEmpty cards) ->
    any gainCard $ history $ performBuyPhase 10 (EvaluationParameters []) (Game.mapState (const (BuyPhase (BuyAllowance buys) ps cards)) (Game.new seed))

  it "completes" $ property $ \seed params (NonEmpty ps) cards ->
    (===) BuyPhaseComplete $ last $ history $ performBuyPhase (min 3 (length cards) + 10) params (Game.mapState (const (BuyPhase (BuyAllowance 3) ps cards)) (Game.new seed))

performBuyPhase :: Int -> EvaluationParameters -> Game -> Game
performBuyPhase limit = execUntil (buyPhaseOver limit)

execUntil :: (Game -> Bool) -> EvaluationParameters -> Game -> Game
execUntil predicate parameters = execState $ runReaderT (Engine.runUntil predicate) parameters

buyPhaseOver :: Int -> Game -> Bool
buyPhaseOver limit = liftA2 (||) (not . inBuyPhase . Game.state) ((> limit) . length . Game.history)

gainCard :: Command -> Bool
gainCard (GainCard _ _) = True
gainCard _ = False

inBuyPhase :: GameState -> Bool
inBuyPhase (BuyPhase _ _ _) = True
inBuyPhase _ = False
