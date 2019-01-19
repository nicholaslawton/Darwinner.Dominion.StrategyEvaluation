module Update (update) where

import Candidate
import Command
import GameState
import GenericPlayer
import PlayerWithoutDominion
import PlayerPreparingStartingDeck
import PlayerDrawingInitialHand
import Player
import Card
import BuyAllowance

import Data.Bool
import Data.List
import Control.Applicative

update :: Command -> GameState -> GameState
update (AddPlayer pid) = addPlayer pid
update MarkPlayersReady = beginPreparingSupply
update (PlaceCardInSupply card) = placeCardInSupply card
update MarkSupplyPrepared = beginPreparingDecks
update (AddCardToDeck pid card) = addCardToDeck pid card
update MarkDecksPrepared = beginDrawingInitialHands
update (DrawCard pid card) = drawCard pid card
update MarkInitialHandsDrawn = beginPlay
update (GainCard pid card) = gainCard pid card
update (DiscardCard pid card) = discardCard pid card
update BuyPhaseComplete = beginCleanUpPhase
update EndGame = const GameOver

addPlayer :: CandidateId -> GameState -> GameState
addPlayer pid (New ps)
  | any ((==) pid . playerId) ps = error "Invalid player join: player already in game"
  | otherwise = New $ PlayerWithoutDominion.new pid : ps
addPlayer _ _ = error "A player may not be added after preparation of the game has commenced"

beginPreparingSupply :: GameState -> GameState
beginPreparingSupply (New pids) = PreparingSupply pids []
beginPreparingSupply _ = error "Cannot prepare the supply of a game which has already begun"

placeCardInSupply :: Card -> GameState -> GameState
placeCardInSupply card (PreparingSupply pids cards) = PreparingSupply pids $ card : cards
placeCardInSupply _ _ = error "A card may only be placed in the supply during game preparation"

beginPreparingDecks :: GameState -> GameState
beginPreparingDecks (PreparingSupply pids cards) =
  PreparingDecks (PlayerPreparingStartingDeck.fromPlayerWithoutDominion <$> pids) cards
beginPreparingDecks _ = error "Deck preparation should occur after the supply has been prepared"

addCardToDeck :: CandidateId -> Card -> GameState -> GameState
addCardToDeck pid card (PreparingDecks ps cards)
  | all ((/=) pid . playerId) ps = error "Invalid deck preparation: player not in game"
  | otherwise = PreparingDecks (alterPlayer (alterDeck (card :)) pid ps) cards
addCardToDeck _ _ _ = error "A card may only be added to a deck during game preparation"

beginDrawingInitialHands :: GameState -> GameState
beginDrawingInitialHands (PreparingDecks ps cards) =
  DrawingInitialHands (PlayerDrawingInitialHand.fromPlayerPreparingStartingDeck <$> ps) cards
beginDrawingInitialHands _ = error "Drawing initial hands must occur after decks have been prepared"

drawCard :: CandidateId -> Card -> GameState -> GameState
drawCard pid card (DrawingInitialHands ps cards)
  | all ((/=) pid . playerId) ps = error "Invalid card draw: player not in game"
  | not $ cardBelongsToPlayer deck playerId card pid ps =
      error "Invalid card draw: card not in deck of player"
  | otherwise =
      DrawingInitialHands
        (alterPlayer
          (PlayerDrawingInitialHand.alterHand (card :) . alterDeck (delete card))
          pid
          ps)
        cards
drawCard _ _ _ = error "A card may only be drawn while players are drawing their initial hands"

beginPlay :: GameState -> GameState
beginPlay (DrawingInitialHands ps cards) =
  BuyPhase BuyAllowance.initial (Player.fromPlayerDrawingInitialHand <$> ps) cards
beginPlay _ = error "Cannot begin play before the game has been fully prepared"

gainCard :: CandidateId -> Card -> GameState -> GameState
gainCard pid card (BuyPhase (BuyAllowance buys) ps cards)
  | not $ playerExists pid ps = error "Invalid card gain: player not in game"
  | notElem card cards = error "Invalid card gain: card not in supply"
  | buys <= 0 = error "Invalid card gain: buy allowance exhausted"
  | otherwise = 
      BuyPhase
        (BuyAllowance (buys - 1))
        (alterPlayer (alterDiscard (card :)) pid ps)
        (delete card cards)
gainCard _ _ _ = error "A card may only be gained during the buy phase"

discardCard :: CandidateId -> Card -> GameState -> GameState
discardCard pid card (CleanUpPhase ps cards)
  | not $ playerExists pid ps = error "Invalid discard: player not in game"
  | not $ cardBelongsToPlayer hand playerId card pid ps =
      error "Invalid discard: card not in hand of player"
  | otherwise = CleanUpPhase (alterPlayer (alterDiscard (card :) . Player.alterHand (delete card)) pid ps) cards
discardCard _ _ _ = error "A card may only be discarded during the clean up phase"

beginCleanUpPhase :: GameState -> GameState
beginCleanUpPhase (BuyPhase _ ps cards) = CleanUpPhase ps cards
beginCleanUpPhase _ = error "Clean up phase must follow buy phase"

alterWhere :: (a -> Bool) -> (a -> a) -> [a] -> [a]
alterWhere p f = fmap $ liftA3 bool id f p

alterElem :: Eq b => (a -> b) -> (a -> a) -> b -> [a] -> [a]
alterElem on f x = alterWhere ((==) x . on) f

alterPlayer :: GenericPlayer p => (p -> p) -> CandidateId -> [p] -> [p]
alterPlayer = alterElem playerId

playerExists :: CandidateId -> [Player] -> Bool
playerExists pid = any ((==) pid . playerId)

cardBelongsToPlayer :: (a -> [Card]) -> (a -> CandidateId) -> Card -> CandidateId -> [a] -> Bool
cardBelongsToPlayer area ppid card pid ps = (elem card . area <$> find ((==) pid . ppid) ps) == Just True
