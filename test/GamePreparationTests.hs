module GamePreparationTests where

import Game
import Player
import Event
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
    (playerIds . mapMaybe playerAdded . history . execUntil prepared params) (Game.new seed) == playerIds candidates

execUntil :: (Game -> Bool) -> EvaluationParameters -> Game -> Game
execUntil predicate parameters = execState $ runReaderT (Engine.runUntil predicate) parameters

prepared :: Game -> Bool
prepared = liftA2 (||) ((== Prepared) . Game.state) ((>100) . length . Game.history)

playerAdded :: Event -> Maybe Player
playerAdded (AddPlayer player) = Just player
playerAdded _ = Nothing

playerIds :: [Player] -> [PlayerId]
playerIds = sort . nub . fmap playerId
