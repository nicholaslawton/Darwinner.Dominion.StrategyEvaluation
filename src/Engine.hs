module Engine (runUntil) where

import Game
import Event

import Control.Monad
import Control.Monad.Trans.State

runUntil :: (Game -> Bool) -> State Game ()
runUntil predicate = do
  game <- get
  unless (predicate game) $ do
    let event = nextEvent game
    let newGame = update event game
    put newGame
    runUntil predicate

nextEvent :: Game -> Event
nextEvent _ = Noop

update :: Event -> Game -> Game
update event game = game { history = event : history game }
