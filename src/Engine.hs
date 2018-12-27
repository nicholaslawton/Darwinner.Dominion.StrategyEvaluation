module Engine (run, runUntil) where

import Card
import Game
import GameState
import Update
import Player
import Command
import EvaluationParameters
import Candidate

import Data.Bool
import Data.List.Unique
import Data.Maybe
import Data.Bifunctor
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import System.Random

run :: ReaderT EvaluationParameters (State Game) ()
run = runUntil ((== Prepared) . Game.state)

runUntil :: (Game -> Bool) -> ReaderT EvaluationParameters (State Game) ()
runUntil predicate = do
  cmd <- nextCommand
  game <- lift $ apply cmd <$> get
  lift $ put game
  unless (predicate game) (runUntil predicate)

apply :: Command -> Game -> Game
apply command = recordCommand command . Game.mapState (update command)

nextCommand :: ReaderT EvaluationParameters (State Game) Command
nextCommand = do
  EvaluationParameters candidates <- ask
  game <- lift get
  case Game.state game of

    New pids ->
      return $ fromMaybe MarkPlayersReady $ AddPlayer <$> nextPlayer
        where
          nextPlayer = listToMaybe $ filter (not . (`elem` pids)) $ candidateId <$> candidates

    PreparingSupply _ cards
      | countElem Copper cards < 60 - length candidates * 7 -> return $ PlaceCardInSupply Copper
      | countElem Silver cards < 40 -> return $ PlaceCardInSupply Silver
      | countElem Gold cards < 30 -> return $ PlaceCardInSupply Gold
      | countElem Estate cards < numVictoryCards candidates -> return $ PlaceCardInSupply Estate
      | countElem Duchy cards < numVictoryCards candidates -> return $ PlaceCardInSupply Duchy
      | countElem Province cards < numVictoryCards candidates -> return $ PlaceCardInSupply Province
      | countElem Curse cards < numCurseCards candidates -> return $ PlaceCardInSupply Curse
      | otherwise -> return MarkSupplyPrepared
        where
          numVictoryCards :: [Candidate] -> Int
          numVictoryCards = bool 8 12 . (> 2) . length
          numCurseCards :: [Candidate] -> Int
          numCurseCards x = case length x of
            2 -> 10
            3 -> 20
            _ -> 30

    PreparingDecks ps _ ->
      return $ fromMaybe MarkDecksPrepared $ uncurry AddCardToDeck <$> playerNeedingCard ps
        where
          playerNeedingCard :: [Player] -> Maybe (CandidateId, Card)
          playerNeedingCard = liftA2 (<|>) (playerNeeding Copper 7) (playerNeeding Estate 3)
          playerNeeding :: Card -> Int -> [Player] -> Maybe (CandidateId, Card)
          playerNeeding card target =
            fmap (flip (,) card . playerId) . listToMaybe . filter ((< target) . countElem card . deck)

    DrawingInitialHands ps _ ->
      fromMaybe (return Noop) $ lift . drawCard <$> playerWithIncompleteHand ps
        where
          playerWithIncompleteHand :: [Player] -> Maybe Player
          playerWithIncompleteHand = listToMaybe . filter ((< 5) . length . hand)
          drawCard :: Player -> State Game Command
          drawCard p = DrawCard (playerId p) <$> randomElement (deck p)

    Prepared -> return Noop

randomElement :: [a] -> State Game a
randomElement xs = do
  game <- get
  let (x, newGen) = first (xs !!) . randomR (0, length xs - 1) . Game.gen $ game
  put $ game { gen = newGen }
  return x
