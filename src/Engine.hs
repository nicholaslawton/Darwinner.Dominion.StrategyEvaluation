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
nextCommand (EvaluationParameters candidates) (PreparingSupply _ cards)
  | length (filter (== Copper) cards) < 60 - length candidates * 7 = PlaceCardInSupply Copper
  | length (filter (== Silver) cards) < 40 = PlaceCardInSupply Silver
  | length (filter (== Gold) cards) < 30 = PlaceCardInSupply Gold
  | length (filter (== Estate) cards) < numVictoryCards = PlaceCardInSupply Estate
  | length (filter (== Duchy) cards) < numVictoryCards = PlaceCardInSupply Duchy
  | length (filter (== Province) cards) < numVictoryCards = PlaceCardInSupply Province
  | otherwise = SupplyReady
    where
      numVictoryCards = if length candidates == 2 then 8 else 12
nextCommand _ (PreparingDecks (p:_) _)
  | length (deck p) < 7 = AddCardToDeck (playerId p) Copper
  | otherwise = AddCardToDeck (playerId p) Copper
nextCommand _ (PreparingDecks [] _) = error "Cannot prepare decks for game with no players"
nextCommand _ Prepared = Noop
