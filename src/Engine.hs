module Engine (run, runUntil) where

import Card
import Game
import GameState
import Update
import Player
import Command
import EvaluationParameters
import Candidate

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
    nextPlayer = listToMaybe $ filter (not . inGame) $ candidateId <$> candidates
    inGame cid = elem cid (playerId <$> ps)
nextCommand (EvaluationParameters candidates) (PreparingSupply _ cards)
  | count Copper cards < 60 - length candidates * 7 = PlaceCardInSupply Copper
  | count Silver cards < 40 = PlaceCardInSupply Silver
  | count Gold cards < 30 = PlaceCardInSupply Gold
  | count Estate cards < numVictoryCards = PlaceCardInSupply Estate
  | count Duchy cards < numVictoryCards = PlaceCardInSupply Duchy
  | count Province cards < numVictoryCards = PlaceCardInSupply Province
  | otherwise = SupplyReady
    where
      numVictoryCards = if length candidates == 2 then 8 else 12
nextCommand _ (PreparingDecks (p:ps) _)
  | count Copper (deck p) < 7 = AddCardToDeck (playerId p) Copper
  | count Estate (deck p) < 3 = AddCardToDeck (playerId p) Estate
  | otherwise = fromMaybe Noop $ flip AddCardToDeck Copper . playerId <$> playerWithEmptyDeck
    where
      playerWithEmptyDeck = listToMaybe $ filter (null . deck) ps
nextCommand _ (PreparingDecks [] _) = error "Cannot prepare decks for game with no players"
nextCommand _ Prepared = Noop

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)
