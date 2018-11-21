module Engine (runUntil) where

import Game

import Control.Monad
import Control.Monad.Trans.State

runUntil :: (GameState -> Bool) -> State Game ()
runUntil _ = void get
