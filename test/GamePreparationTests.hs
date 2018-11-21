module GamePreparationTests where

import Game
import Player
import Event
import Engine

import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad.Trans.State

import ArbitraryInstances()
import Test.Hspec
import Test.QuickCheck

gamePreparationTests :: SpecWith ()
gamePreparationTests = describe "game preparation" $ do

  it "adds all players" $ property $ \seed players ->
    (playerIds . mapMaybe playerAdded . Game.history . execUntil prepared) (Game.new seed) == playerIds players

execUntil :: (Game -> Bool) -> Game -> Game
execUntil predicate = execState (Engine.runUntil predicate)

prepared :: Game -> Bool
prepared = liftA2 (||) ((== Prepared) . Game.state) ((>10) . length . Game.history)

playerAdded :: Event -> Maybe Player
playerAdded Noop = Nothing

playerIds :: [Player] -> [PlayerId]
playerIds = sort . fmap playerId
