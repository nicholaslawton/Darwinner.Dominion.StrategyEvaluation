module EngineValidation (execUntil, execPhase, inState) where

import Game
import GameState
import EvaluationParameters
import Engine

import Control.Applicative
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

execUntil :: (Game -> Bool) -> EvaluationParameters -> Game -> Game
execUntil predicate parameters = execState $ runReaderT (Engine.runUntil predicate) parameters

execWhile :: (Game -> Bool) -> EvaluationParameters -> Game -> Game
execWhile predicate = execUntil (not . predicate)

execPhase :: (GameState -> Bool) -> Int -> EvaluationParameters -> Game -> Game
execPhase phase = execWhile . inState phase

inState :: (GameState -> Bool) -> Int -> Game -> Bool
inState predicate limit = liftA2 (||) (predicate . Game.state) ((> limit) . length . Game.history)
