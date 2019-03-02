module Engine (run, runUntil) where

import Card
import Game
import GameState
import PlayState
import Update
import Player
import Command
import EvaluationParameters
import Candidate
import BuyAllowance

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
run = runUntil ((== GameOver) . Game.state)

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
      lift $ maybe MarkPlayersReady AddPlayer <$> nextPlayer (candidateId <$> candidates)
        where
          nextPlayer :: [CandidateId] -> State Game (Maybe CandidateId)
          nextPlayer = randomElement . filter (\cid -> all ((/=) cid . playerId) pids)

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
          numCurseCards = (* 10) . subtract 1 . length

    PreparingDecks ps _ ->
      return $ maybe MarkDecksPrepared (uncurry AddCardToDeck) (playerNeedingCard ps)
        where
          playerNeedingCard :: Player p => [p] -> Maybe (CandidateId, Card)
          playerNeedingCard = liftA2 (<|>) (playerNeeding Copper 7) (playerNeeding Estate 3)
          playerNeeding :: Player p => Card -> Int -> [p] -> Maybe (CandidateId, Card)
          playerNeeding card target =
            fmap (flip (,) card . playerId) . listToMaybe . filter ((< target) . countElem card . deck)

    DrawingInitialHands ps _ ->
      maybe (return MarkInitialHandsDrawn) (lift . drawCard unexpected) (playerWithIncompleteHand ps)
        where
          playerWithIncompleteHand :: Player p => [p] -> Maybe p
          playerWithIncompleteHand = listToMaybe . filter ((< 5) . length . hand)

          unexpected = error "Unexpected failure drawing a card for initial hand"

    BuyPhase (BuyAllowance buys) _ | buys <= 0 -> return BuyPhaseComplete

    BuyPhase _ playState ->
      return $ maybe BuyPhaseComplete (GainCard $ playerId p) (listToMaybe $ PlayState.supply playState)
        where
          p = activePlayer playState

    CleanUpPhase Discard playState ->
      return $ maybe DiscardStepComplete (DiscardCard $ playerId p) (listToMaybe $ hand p)
        where
          p = activePlayer playState

    CleanUpPhase DrawHand playState ->
      if length (hand p) < 5 then lift $ drawCard CleanUpPhaseComplete p else return CleanUpPhaseComplete
        where
          p = activePlayer playState

    GameOver -> error "Game is over"

drawCard :: Player p => Command -> p -> State Game Command
drawCard alternateCommand p
  | not $ null $ deck p = maybe unexpected (DrawCard $ playerId p) <$> randomElement (deck p)
  | not $ null $ discard p = return $ ReformDeck $ playerId p
  | otherwise = return alternateCommand
    where unexpected = error "Unexpected failure drawing a card from a non-empty deck"

randomElement :: [a] -> State Game (Maybe a)
randomElement [] = return Nothing
randomElement xs = do
  game <- get
  let (x, newGen) = first (Just . (xs !!)) . randomR (0, length xs - 1) . Game.gen $ game
  put $ game { gen = newGen }
  return x
