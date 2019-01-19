module Engine (run, runUntil) where

import Card
import Game
import GameState
import Update
import GenericPlayer
import PlayerPreparingStartingDeck
import PlayerDrawingInitialHand
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
          nextPlayer = randomElement . filter (\cid -> all ((/=) cid . GenericPlayer.playerId) pids)

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
      return $ fromMaybe MarkDecksPrepared $ uncurry AddCardToDeck <$> playerNeedingCard ps
        where
          playerNeedingCard :: [PlayerPreparingStartingDeck] -> Maybe (CandidateId, Card)
          playerNeedingCard = liftA2 (<|>) (playerNeeding Copper 7) (playerNeeding Estate 3)
          playerNeeding :: Card -> Int -> [PlayerPreparingStartingDeck] -> Maybe (CandidateId, Card)
          playerNeeding card target =
            fmap (flip (,) card . GenericPlayer.playerId)
              . listToMaybe
              . filter ((< target) . countElem card . GenericPlayer.deck)

    DrawingInitialHands ps _ ->
      fromMaybe (return MarkInitialHandsDrawn) $ lift . drawCard <$> playerWithIncompleteHand ps
        where
          playerWithIncompleteHand :: [PlayerDrawingInitialHand] -> Maybe PlayerDrawingInitialHand
          playerWithIncompleteHand = listToMaybe . filter ((< 5) . length . PlayerDrawingInitialHand.hand)
          drawCard :: PlayerDrawingInitialHand -> State Game Command
          drawCard p =
            maybe emptyDeckError (DrawCard (GenericPlayer.playerId p))
              <$> randomElement (PlayerDrawingInitialHand.deck p)
          emptyDeckError = error "unexpected empty deck when drawing card for initial hand"

    BuyPhase (BuyAllowance buys) _ _ | buys <= 0 -> return BuyPhaseComplete

    BuyPhase _ (p:_) (card:_) -> return $ GainCard (GenericPlayer.playerId p) card

    BuyPhase _ [] _ -> error "Unexpected game in progress with no players"

    BuyPhase _ _ [] -> return BuyPhaseComplete -- error "Unexpected empty supply while game in progress"

    CleanUpPhase _ _ -> return EndGame

    GameOver -> error "Game is over"

randomElement :: [a] -> State Game (Maybe a)
randomElement [] = return Nothing
randomElement xs = do
  game <- get
  let (x, newGen) = first (Just . (xs !!)) . randomR (0, length xs - 1) . Game.gen $ game
  put $ game { gen = newGen }
  return x
