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
    parameters <- ask
    let (cmd, newGen) = nextCommand parameters (Game.state game) (gen game)
    lift . put $ apply cmd (game { gen = newGen })
    runUntil predicate

apply :: Command -> Game -> Game
apply command = recordCommand command . Game.mapState (update command)

nextCommand :: EvaluationParameters -> GameState -> StdGen -> (Command, StdGen)
nextCommand (EvaluationParameters candidates) (New ps) g =
  flip (,) g $ fromMaybe MarkPlayersReady $ AddPlayer <$> nextPlayer
    where
      nextPlayer = listToMaybe $ filter (not . inGame) $ candidateId <$> candidates
      inGame cid = elem cid (playerId <$> ps)
nextCommand (EvaluationParameters candidates) (PreparingSupply _ cards) g
  | countElem Copper cards < 60 - length candidates * 7 = (PlaceCardInSupply Copper, g)
  | countElem Silver cards < 40 = (PlaceCardInSupply Silver, g)
  | countElem Gold cards < 30 = (PlaceCardInSupply Gold, g)
  | countElem Estate cards < numVictoryCards = (PlaceCardInSupply Estate, g)
  | countElem Duchy cards < numVictoryCards = (PlaceCardInSupply Duchy, g)
  | countElem Province cards < numVictoryCards = (PlaceCardInSupply Province, g)
  | otherwise = (MarkSupplyPrepared, g)
    where
      numVictoryCards = if length candidates == 2 then 8 else 12
nextCommand _ (PreparingDecks ps _) g =
  flip (,) g $ fromMaybe MarkDecksPrepared $ uncurry AddCardToDeck <$> playerNeedingCard ps
    where
      playerNeedingCard :: [Player] -> Maybe (CandidateId, Card)
      playerNeedingCard = liftA2 (<|>) (playerNeeding Copper 7) (playerNeeding Estate 3)
      playerNeeding :: Card -> Int -> [Player] -> Maybe (CandidateId, Card)
      playerNeeding card target =
        fmap (flip (,) card . playerId) . listToMaybe . filter ((< target) . countElem card . deck)
nextCommand _ (DrawingInitialHands ps _) g =
  fromMaybe (Noop, g) $ flip drawCard g <$> playerWithIncompleteHand ps
    where
      playerWithIncompleteHand :: [Player] -> Maybe Player
      playerWithIncompleteHand = listToMaybe . filter ((< 5) . length . hand)
      drawCard :: Player -> StdGen -> (Command, StdGen)
      drawCard p = first (DrawCard (playerId p)) . randomElement (deck p)
      randomElement :: [a] -> StdGen -> (a, StdGen)
      randomElement xs = first (xs !!) . randomR (0, length xs)
nextCommand _ Prepared g = (Noop, g)
