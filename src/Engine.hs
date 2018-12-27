module Engine (run, runUntil) where

import Card
import Game
import GameState
import Update
import Player
import Command
import EvaluationParameters
import Candidate

import ListExtension

import Data.Maybe
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

run :: ReaderT EvaluationParameters (State Game) ()
run = runUntil ((== Prepared) . Game.state)

runUntil :: (Game -> Bool) -> ReaderT EvaluationParameters (State Game) ()
runUntil predicate = do
  game <- lift get
  unless (predicate game) $ do
    parameters <- ask
    lift . put $ apply (nextCommand parameters (Game.state game)) game
    runUntil predicate

apply :: Command -> Game -> Game
apply command = recordCommand command . Game.mapState (update command)

nextCommand :: EvaluationParameters -> GameState -> Command
nextCommand (EvaluationParameters candidates) (New ps) = fromMaybe MarkPlayersReady $ AddPlayer <$> nextPlayer
  where
    nextPlayer = listToMaybe $ filter (not . inGame) $ candidateId <$> candidates
    inGame cid = elem cid (playerId <$> ps)
nextCommand (EvaluationParameters candidates) (PreparingSupply _ cards)
  | count Copper cards < 60 - length candidates * 7 = PlaceCardInSupply Copper
  | count Silver cards < 40 = PlaceCardInSupply Silver
  | count Gold cards < 30 = PlaceCardInSupply Gold
  | count Estate cards < numVictoryCards = PlaceCardInSupply Estate
  | count Duchy cards < numVictoryCards = PlaceCardInSupply Duchy
  | count Province cards < numVictoryCards = PlaceCardInSupply Province
  | otherwise = MarkSupplyPrepared
    where
      numVictoryCards = if length candidates == 2 then 8 else 12
nextCommand _ (PreparingDecks ps _) = fromMaybe MarkDecksPrepared $ uncurry AddCardToDeck <$> recipientAndCard
  where
    recipientAndCard :: Maybe (CandidateId, Card)
    recipientAndCard = recipientNeedingCard Copper 7 <|> recipientNeedingCard Estate 3
    recipientNeedingCard :: Card -> Int -> Maybe (CandidateId, Card)
    recipientNeedingCard card target =
      flip (,) card . playerId <$> listToMaybe (filter ((< target) . count card . deck) ps)
nextCommand _ (DrawingInitialHands (p:_) _) = DrawCard (playerId p) Copper
nextCommand _ (DrawingInitialHands [] _) = error "Cannot draw initial hands for a game with no players"
nextCommand _ Prepared = Noop
