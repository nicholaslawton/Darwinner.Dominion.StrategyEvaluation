module GamePreparationTests where

import Game
import GameState
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

  it "gives 7 coppers to each player" $ property $ givesCardsToEachPlayer 7 Copper

  it "gives 3 estates to each player" $ property $ givesCardsToEachPlayer 3 Estate

  it "puts all coppers in play" $ property $
    (===) 60 . count Copper . mapMaybe cardPutInPlay . history . uncurry prepareGame

prepareGame :: Int -> EvaluationParameters -> Game
prepareGame seed params = execUntil prepared params (Game.new seed)

execUntil :: (Game -> Bool) -> EvaluationParameters -> Game -> Game
execUntil predicate parameters = execState $ runReaderT (Engine.runUntil predicate) parameters

prepared :: Game -> Bool
prepared = liftA2 (||) ((== Prepared) . Game.state) ((>200) . length . Game.history)

categorise :: Ord k => (a -> k) -> (a -> v) -> [a] -> Map k [v]
categorise key value xs = fromListWith (++) $ liftA2 (,) key ((: []) . value) <$> xs

candidateIds :: [Candidate] -> [CandidateId]
candidateIds = sort . nub . fmap candidateId

playerAdded :: Command -> Maybe CandidateId
playerAdded (AddPlayer pid) = Just pid
playerAdded _ = Nothing

cardPlacedInSupply :: Command -> Maybe Card
cardPlacedInSupply (PlaceCardInSupply card) = Just card
cardPlacedInSupply _ = Nothing

cardAddedToDeck :: Command -> Maybe (CandidateId, Card)
cardAddedToDeck (AddCardToDeck pid card) = Just (pid, card)
cardAddedToDeck _ = Nothing

cardPutInPlay :: Command -> Maybe Card
cardPutInPlay (PlaceCardInSupply card) = Just card
cardPutInPlay (AddCardToDeck _ card) = Just card
cardPutInPlay _ = Nothing

givesCardsToEachPlayer :: Int -> Card -> Int -> EvaluationParameters -> Property
givesCardsToEachPlayer n card seed (params@(EvaluationParameters candidates)) =
  fromList (flip (,) n . candidateId <$> candidates)
    ===
      ( fmap length
      . categorise fst snd
      . filter ((==) card . snd)
      . mapMaybe cardAddedToDeck
      . history
      . prepareGame seed
      ) params
