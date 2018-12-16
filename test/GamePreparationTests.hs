module GamePreparationTests where

import Game
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

  it "places copper in supply" $ property $ 
    elem Copper . mapMaybe cardPlacedInSupply . history . uncurry prepareGame

prepareGame :: Int -> EvaluationParameters -> Game
prepareGame seed params = execUntil prepared params (Game.new seed)

execUntil :: (Game -> Bool) -> EvaluationParameters -> Game -> Game
execUntil predicate parameters = execState $ runReaderT (Engine.runUntil predicate) parameters

prepared :: Game -> Bool
prepared = liftA2 (||) ((== Prepared) . Game.state) ((>100) . length . Game.history)

playerIds :: [Player] -> [PlayerId]
playerIds = sort . nub . fmap playerId

playerAdded :: Event -> Maybe Player
playerAdded (AddPlayer player) = Just player
playerAdded _ = Nothing

cardPlacedInSupply :: Event -> Maybe Card
cardPlacedInSupply (PlaceCardInSupply card) = Just card
cardPlacedInSupply _ = Nothing
