module Engine (run, runUntil) where

import Card
import Game
import GameState
import Update
import Player
import Command
import EvaluationParameters
import Candidate

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
  game <- lift get
  unless (predicate game) $ do
    cmd <- nextCommand
    lift . put $ apply cmd game
    runUntil predicate

apply :: Command -> Game -> Game
apply command = recordCommand command . Game.mapState (update command)

nextCommand :: ReaderT EvaluationParameters (State Game) Command
nextCommand = do
  EvaluationParameters candidates <- ask
  game <- lift get
  case Game.state game of

    New ps ->
      return $ fromMaybe MarkPlayersReady $ AddPlayer <$> nextPlayer
        where
          nextPlayer = listToMaybe $ filter (not . inGame) $ candidateId <$> candidates
          inGame cid = elem cid (playerId <$> ps)

    PreparingSupply _ cards
      | countElem Copper cards < 60 - length candidates * 7 -> return $ PlaceCardInSupply Copper
      | countElem Silver cards < 40 -> return $ PlaceCardInSupply Silver
      | countElem Gold cards < 30 -> return $ PlaceCardInSupply Gold
      | countElem Estate cards < numVictoryCards -> return $ PlaceCardInSupply Estate
      | countElem Duchy cards < numVictoryCards -> return $ PlaceCardInSupply Duchy
      | countElem Province cards < numVictoryCards -> return $ PlaceCardInSupply Province
      | otherwise -> return $ MarkSupplyPrepared
        where
          numVictoryCards = if length candidates == 2 then 8 else 12

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
  let (x, newGen) = first (xs !!) . randomR (0, length xs) . Game.gen $ game
  put $ game { gen = newGen }
  return x
