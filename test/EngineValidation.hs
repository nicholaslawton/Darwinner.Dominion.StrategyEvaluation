module EngineValidation (execUntil) where

import Game
import EvaluationParameters
import Engine

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

execUntil :: (Game -> Bool) -> EvaluationParameters -> Game -> Game
execUntil predicate parameters = execState $ runReaderT (Engine.runUntil predicate) parameters
