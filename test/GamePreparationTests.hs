module GamePreparationTests where

import Game
import GameState
import Player
import Card
import Command
import Engine
import EvaluationParameters
import Candidate

import Data.List
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

  it "adds 3 estates and 7 coppers to a deck" $ property $
    (=== [(Estate, 3), (Copper, 7)])
      . counts
      . fmap snd
      . maximumBy (\a b -> compare (length a) (length b))
      . groupBy (\a b -> fst a == fst b)
      . sortOn fst
      . mapMaybe cardAddedToDeck
      . history
      . uncurry prepareGame

  it "prepares deck of only one player" $ property $ \seed (params@(EvaluationParameters _candidates)) ->
    (length . sort . nub . mapMaybe (fmap fst . cardAddedToDeck) . history . prepareGame seed) params === 1 --candidatesIds candidates

prepareGame :: Int -> EvaluationParameters -> Game
prepareGame seed params = execUntil prepared params (Game.new seed)

execUntil :: (Game -> Bool) -> EvaluationParameters -> Game -> Game
execUntil predicate parameters = execState $ runReaderT (Engine.runUntil predicate) parameters

prepared :: Game -> Bool
prepared = liftA2 (||) ((== Prepared) . Game.state) ((>200) . length . Game.history)

counts :: Eq a => [a] -> [(a, Int)]
counts = sortOn snd . fmap (liftA2 (,) head length) . group

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
