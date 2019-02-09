module EngineValidation
  ( execUntil
  , execWhile
  ) where

import Game
import GameState
import EvaluationParameters
import Engine

import Control.Applicative
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

execUntil :: (GameState -> Bool) -> Int -> EvaluationParameters -> Game -> Game
execUntil predicate limit parameters = execState $ runReaderT (Engine.runUntil (inState predicate limit)) parameters

execWhile :: (GameState -> Bool) -> Int -> EvaluationParameters -> Game -> Game
execWhile predicate = execUntil (not . predicate)

inState :: (GameState -> Bool) -> Int -> Game -> Bool
inState predicate limit = liftA2 (||) (predicate . Game.state) ((> limit) . length . Game.history)
