module GamePreparationTests where

import Game
import Player
import Event
import Engine

import Data.List
import Data.Maybe
import Control.Monad.Trans.State

import ArbitraryInstances
import Test.Hspec
import Test.QuickCheck

gamePreparationTests :: SpecWith ()
gamePreparationTests = describe "game preparation" $ do

  it "adds all players" $ property $ \seed players ->
    (playersAdded . Game.history . execUntil (== Prepared)) (Game.new seed)
      == sort (playerId <$> players)

execUntil :: (GameState -> Bool) -> Game -> Game
execUntil predicate = execState (Engine.runUntil predicate)

playersAdded :: [Event] -> [PlayerId]
playersAdded = sort . fmap playerId . mapMaybe playerAdded

playerAdded :: Event -> Maybe Player
playerAdded Noop = Nothing
