module GamePreparationTests where

import Game
import GameState
import Player
import Card
import Command
import Engine
import EvaluationParameters
import Candidate

import ListExtension

import Data.List
import Data.Map (Map, fromList, fromListWith)
import Data.Maybe
import Control.Applicative
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

import ArbitraryInstances()
import Test.Hspec
import Test.QuickCheck

gamePreparationTests :: SpecWith ()
gamePreparationTests = describe "game preparation" $ do

  it "adds all players" $ property $ \seed (params@(EvaluationParameters candidates)) ->
    (sort . mapMaybe playerAdded . history . prepareGame seed) params === candidateIds candidates

  it "places treasure and victory cards in supply" $ property $ 
    let expected = [Copper, Silver, Gold, Estate, Duchy, Province]
    in (===) expected . intersect expected . nub . mapMaybe cardPlacedInSupply . history . uncurry prepareGame

  it "gives 7 coppers per player" $ property $ \seed (params@(EvaluationParameters candidates)) ->
    length candidates * 7
      === (count Copper . mapMaybe (fmap snd . cardAddedToDeck) . history . prepareGame seed) params

  it "gives 3 estates per player" $ property $ \seed (params@(EvaluationParameters candidates)) ->
    length candidates * 3
      === (count Estate . mapMaybe (fmap snd . cardAddedToDeck) . history . prepareGame seed) params

  it "gives 10 cards to each player" $ property $ \seed (params@(EvaluationParameters candidates)) ->
    fromList (flip (,) 10 <$> candidateIds candidates)
      === (fmap length . categorise fst snd . mapMaybe cardAddedToDeck . history . prepareGame seed) params

prepareGame :: Int -> EvaluationParameters -> Game
prepareGame seed params = execUntil prepared params (Game.new seed)

execUntil :: (Game -> Bool) -> EvaluationParameters -> Game -> Game
execUntil predicate parameters = execState $ runReaderT (Engine.runUntil predicate) parameters

prepared :: Game -> Bool
prepared = liftA2 (||) ((== Prepared) . Game.state) ((>200) . length . Game.history)

categorise :: Ord k => (a -> k) -> (a -> v) -> [a] -> Map k [v]
categorise key value xs = fromListWith (++) $ liftA2 (,) key ((: []) . value) <$> xs

candidateIds :: [Candidate] -> [PlayerId]
candidateIds = sort . nub . fmap candidateId

playerAdded :: Command -> Maybe PlayerId
playerAdded (AddPlayer pid) = Just pid
playerAdded _ = Nothing

cardPlacedInSupply :: Command -> Maybe Card
cardPlacedInSupply (PlaceCardInSupply card) = Just card
cardPlacedInSupply _ = Nothing

cardAddedToDeck :: Command -> Maybe (PlayerId, Card)
cardAddedToDeck (AddCardToDeck pid card) = Just (pid, card)
cardAddedToDeck _ = Nothing
