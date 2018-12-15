module Engine (runUntil) where

import Card
import Game
import Player
import Event
import EvaluationParameters

import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

runUntil :: (Game -> Bool) -> ReaderT EvaluationParameters (State Game) ()
runUntil predicate = do
  game <- lift get
  unless (predicate game) $ do
    parameters <- ask
    lift . put $ apply (nextEvent parameters (Game.state game)) game
    runUntil predicate

apply :: Event -> Game -> Game
apply event = record event . updateState (update event)

record :: Event -> Game -> Game
record event game = game { history = event : history game }

nextEvent :: EvaluationParameters -> GameState -> Event
nextEvent (EvaluationParameters candidates) (New ps) = fromMaybe Noop $ AddPlayer <$> nextPlayer
  where
    nextPlayer = listToMaybe $ filter (not . playerInGame) candidates
    playerInGame player = playerId player `elem` (playerId <$> ps)
nextEvent _ (PreparingSupply _ _) = Noop
nextEvent _ Prepared = Noop

update :: Event -> GameState -> GameState
update (AddPlayer player) = addPlayer player
update (AddCardToSupply card) = addCardToSupply card
update Noop = id

updateState :: (GameState -> GameState) -> Game -> Game
updateState f game = game { Game.state = f (Game.state game) }

addPlayer :: Player -> GameState -> GameState
addPlayer player (New ps) = New $ player : ps
addPlayer _ _ = error "A player may not be added after preparation of the game has commenced"

addCardToSupply :: Card -> GameState -> GameState
addCardToSupply card (New ps) = PreparingSupply ps [card]
addCardToSupply card (PreparingSupply ps supply) = PreparingSupply ps $ card : supply
addCardToSupply _ _ = error "A card may not be added to the supply after supply preparation has completed"
