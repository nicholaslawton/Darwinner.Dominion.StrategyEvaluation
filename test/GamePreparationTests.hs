module GamePreparationTests where

import Game
import Card
import EvaluationParameters
import Candidate

import Data.List
import Data.List.Unique
import Data.Bifunctor
import Data.Map hiding (mapMaybe)
import Data.Maybe

import GameStateValidation
import EngineValidation
import CommandValidation
import Categorisation
import ArbitraryInstances()
import CardOrder
import Test.Hspec
import Test.QuickCheck

gamePreparationTests :: SpecWith ()
gamePreparationTests = describe "game preparation" $ do

  it "adds all players" $ property $ \seed (params@(EvaluationParameters candidates)) ->
    (sort . mapMaybe playerAdded . history . prepareGame seed) params === (sort . fmap candidateId) candidates

  it "places treasure and victory cards in supply" $ property $ 
    let expected = [Copper, Silver, Gold, Estate, Duchy, Province, Curse]
    in (===) expected . intersect expected . nub . mapMaybe cardPlacedInSupply . history . uncurry prepareGame

  it "places correct number of victory cards in supply" $ property $
    \seed (params@(EvaluationParameters candidates)) ->
      (countElem Province . mapMaybe cardPlacedInSupply . history . prepareGame seed) params
        ===
          case length candidates of
            2 -> 8
            3 -> 12
            4 -> 12
            _ -> error "Unexpected number of players"

  it "places correct number of curse cards in supply" $ property $
    \seed (params@(EvaluationParameters candidates)) ->
      (countElem Curse . mapMaybe cardPlacedInSupply . history . prepareGame seed) params
        ===
          case length candidates of
            2 -> 10
            3 -> 20
            4 -> 30
            _ -> error "Unexpected number of players"

  it "prepares starting deck for each player" $ property $ \seed (params@(EvaluationParameters candidates)) ->
    let startingDeck = fromList $ first arbitraryCardOrder <$> [(Copper, 7), (Estate, 3)]
    in
      fromList (flip (,) startingDeck . candidateId <$> candidates)
        ===
          ( fmap (fmap length . categorise arbitraryCardOrder id)
          . categorise fst snd
          . mapMaybe cardAddedToDeck
          . history
          . prepareGame seed
          ) params

  it "puts all coppers in play" $ property $
    (===) 60 . countElem Copper . mapMaybe cardPutInPlay . history . uncurry prepareGame

  it "draws initial hand for each player" $ property $ \seed (params@(EvaluationParameters candidates)) ->
    fromList (flip (,) 5 . candidateId <$> candidates)
      === (fmap length . categorise fst snd . mapMaybe cardDrawn . history . prepareGame seed) params

  it "begins first turn" $ property $
    buyPhase . state . uncurry prepareGame

prepareGame :: Int -> EvaluationParameters -> Game
prepareGame seed params = execUntil buyPhase 300 params (Game.new seed)
