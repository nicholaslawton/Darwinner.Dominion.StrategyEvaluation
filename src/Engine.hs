module Engine (run, runUntil) where

import Card
import Game
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
apply command = recordCommand command . updateState (update command)

updateState :: (GameState -> GameState) -> Game -> Game
updateState f game = game { Game.state = f (Game.state game) }

nextCommand :: EvaluationParameters -> GameState -> Command
nextCommand (EvaluationParameters candidates) (New ps) = fromMaybe PlayersReady $ AddPlayer <$> nextPlayer
  where
    nextPlayer = listToMaybe $ filter (not . playerInGame) candidates
    playerInGame player = playerId player `elem` (playerId <$> ps)
nextCommand (EvaluationParameters candidates) (PreparingSupply _ supply)
  | length (filter (== Copper) supply) < 60 - length candidates * 7 = PlaceCardInSupply Copper
  | length (filter (== Silver) supply) < 40 = PlaceCardInSupply Silver
  | length (filter (== Gold) supply) < 30 = PlaceCardInSupply Gold
  | otherwise = Noop
nextCommand _ Prepared = Noop

update :: Command -> GameState -> GameState
update (AddPlayer player) = addPlayer player
update PlayersReady = beginPreparingSupply
update (PlaceCardInSupply card) = placeCardInSupply card
update Noop = const Prepared

addPlayer :: Player -> GameState -> GameState
addPlayer player (New ps) = New $ player : ps
addPlayer _ _ = error "A player may not be added after preparation of the game has commenced"

beginPreparingSupply :: GameState -> GameState
beginPreparingSupply (New ps) = PreparingSupply ps []
beginPreparingSupply _ = error "Cannot prepare the supply of a game which has already begun"

placeCardInSupply :: Card -> GameState -> GameState
placeCardInSupply card (PreparingSupply ps supply) = PreparingSupply ps $ card : supply
placeCardInSupply _ _ = error "A card may only be placed in the supply during preparation"
