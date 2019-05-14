module CleanUpPhaseTests (cleanUpPhaseTests) where

import Game
import GameState
import PlayState
import EvaluationParameters
import Player
import CompletePlayer

import Data.List
import Data.Maybe
import Data.Composition

import GameStateValidation
import EngineValidation
import EventValidation
import PlayerValidation
import ArbitraryInstances()
import CardOrder
import Test.Hspec
import Test.QuickCheck hiding (Discard)

cleanUpPhaseTests :: SpecWith ()
cleanUpPhaseTests = describe "clean up phase" $ do
  
  it "discards all cards from hand" $ property $ \params (NonEmpty ps) cards t ->
    let
      g = PlayState ps cards t
    in
      (===) (sortOn arbitraryCardOrder . hand $ activePlayer g)
        . sortOn arbitraryCardOrder
        . mapMaybe unplayedCardDiscarded
        . history
        . runTest params Discard g

  it "discards all played cards" $ property $ \params (NonEmpty ps) cards t ->
    let
      g = PlayState ps cards t
    in
      (===) (sortOn arbitraryCardOrder . playedCards $ activePlayer g)
        . sortOn arbitraryCardOrder
        . mapMaybe playedCardDiscarded
        . history
        . runTest params Discard g

  it "draws new hand" $ property $ \params (NonEmpty ps) cards t ->
    let
      g = PlayState ps cards t
    in
      (===) (min 5 . length . dominion $ activePlayer g)
        . length
        . filter ((/=) Nothing . cardDrawn)
        . history
        . runTest params Discard g

runTest :: EvaluationParameters -> CleanUpStep -> PlayState -> Int -> Game
runTest params step g@(PlayState ps _ _) = execWhile cleanUpPhase (actionLimit ps) params . gameInCleanUpPhase step g

actionLimit :: [CompletePlayer] -> Int
actionLimit = (*2) . sum . fmap (length . dominion)

gameInCleanUpPhase :: CleanUpStep -> PlayState -> Int -> Game
gameInCleanUpPhase = gameInState .: CleanUpPhase
