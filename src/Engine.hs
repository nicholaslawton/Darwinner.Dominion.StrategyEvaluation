module Engine (run, runUntil) where

import Card
import Game
import GameState
import Update
import Player
import Command
import EvaluationParameters

import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

run :: ReaderT EvaluationParameters (State Game) ()
run = runUntil ((== Prepared) . Game.state)

runUntil :: (Game -> Bool) -> ReaderT EvaluationParameters (State Game) ()
runUntil predicate = do
  game <- lift get
  unless (predicate game) $ do
    parameters <- ask
    lift . put $ apply (nextCommand parameters (Game.state game)) game
    runUntil predicate

apply :: Command -> Game -> Game
apply command = recordCommand command . Game.mapState (update command)

nextCommand :: EvaluationParameters -> GameState -> Command
nextCommand (EvaluationParameters candidates) (New ps) = fromMaybe PlayersReady $ AddPlayer <$> nextPlayer
  where
    nextPlayer = listToMaybe $ filter (not . playerInGame) candidates
    playerInGame player = playerId player `elem` (playerId <$> ps)
nextCommand (EvaluationParameters candidates) (PreparingSupply _ supply)
  | length (filter (== Copper) supply) < 60 - length candidates * 7 = PlaceCardInSupply Copper
  | length (filter (== Silver) supply) < 40 = PlaceCardInSupply Silver
  | length (filter (== Gold) supply) < 30 = PlaceCardInSupply Gold
  | length (filter (== Estate) supply) < numVictoryCards = PlaceCardInSupply Estate
  | length (filter (== Duchy) supply) < numVictoryCards = PlaceCardInSupply Duchy
  | length (filter (== Province) supply) < numVictoryCards = PlaceCardInSupply Province
  | otherwise = Noop
    where
      numVictoryCards = if length candidates == 2 then 8 else 12
nextCommand _ Prepared = Noop
