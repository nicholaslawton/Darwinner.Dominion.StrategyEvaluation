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
    let event = nextEvent parameters game
    let newGame = apply event game
    lift $ put newGame
    runUntil predicate

nextEvent :: EvaluationParameters -> Game -> Event
nextEvent (EvaluationParameters candidates) game = fromMaybe Noop $ AddPlayer <$> nextPlayer
  where
    nextPlayer = listToMaybe $ filter (not . playerInGame) candidates
    playerInGame player = playerId player `elem` (playerId <$> players game)

apply :: Event -> Game -> Game
apply event game = record event (updateState (update event) game)

record :: Event -> Game -> Game
record event game = game { history = event : history game }

update :: Event -> GameState -> GameState
update (AddPlayer player) = addPlayer player
update (AddCardToSupply card) = addCardToSupply card
update _ = id

updateState :: (GameState -> GameState) -> Game -> Game
updateState f game = game { Game.state = f (Game.state game) }

addPlayer :: Player -> GameState -> GameState
addPlayer player (New ps) = New $ player : ps
addPlayer _ game = game

addCardToSupply :: Card -> GameState -> GameState
addCardToSupply card (PreparingSupply ps supply) = PreparingSupply ps $ card : supply
addCardToSupply _ game = game
