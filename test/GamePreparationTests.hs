module GamePreparationTests where

import Game
import GameState
import Player
import Card
import Command
import Engine
import EvaluationParameters

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
    (playerIds . mapMaybe playerAdded . history . prepareGame seed) params == playerIds candidates

  it "places treasure and victory cards in supply" $ property $ 
    let expected = [Copper, Silver, Gold, Estate, Duchy, Province]
    in (==) expected . intersect expected . nub . mapMaybe cardPlacedInSupply . history . uncurry prepareGame

  it "adds at least 7 coppers to a deck" $ property $
    (>=7) . length . filter (== Copper) . mapMaybe cardAddedToDeck . history . uncurry prepareGame

prepareGame :: Int -> EvaluationParameters -> Game
prepareGame seed params = execUntil prepared params (Game.new seed)

execUntil :: (Game -> Bool) -> EvaluationParameters -> Game -> Game
execUntil predicate parameters = execState $ runReaderT (Engine.runUntil predicate) parameters

prepared :: Game -> Bool
prepared = liftA2 (||) ((== Prepared) . Game.state) ((>200) . length . Game.history)

playerIds :: [Player] -> [PlayerId]
playerIds = sort . nub . fmap playerId

playerAdded :: Command -> Maybe Player
playerAdded (AddPlayer player) = Just player
playerAdded _ = Nothing

cardPlacedInSupply :: Command -> Maybe Card
cardPlacedInSupply (PlaceCardInSupply card) = Just card
cardPlacedInSupply _ = Nothing

cardAddedToDeck :: Command -> Maybe Card
cardAddedToDeck (AddCardToDeck _ card) = Just card
cardAddedToDeck _ = Nothing
