module Update (update) where

import Candidate
import Command
import GameState
import PlayerPreparingStartingDeck
import PlayerDrawingInitialHand
import Player
import Card

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
update BuyPhaseComplete = beginCleanUpPhase
update Noop = id

addPlayer :: CandidateId -> GameState -> GameState
addPlayer pid (New pids)
  | elem pid pids = error "Invalid player join: player already in game"
  | otherwise = New $ pid : pids
addPlayer _ _ = error "A player may not be added after preparation of the game has commenced"

beginPreparingSupply :: GameState -> GameState
beginPreparingSupply (New pids) = PreparingSupply pids []
beginPreparingSupply _ = error "Cannot prepare the supply of a game which has already begun"

placeCardInSupply :: Card -> GameState -> GameState
placeCardInSupply card (PreparingSupply pids cards) = PreparingSupply pids $ card : cards
placeCardInSupply _ _ = error "A card may only be placed in the supply during game preparation"

beginPreparingDecks :: GameState -> GameState
beginPreparingDecks (PreparingSupply pids cards) =
  PreparingDecks (PlayerPreparingStartingDeck.new <$> pids) cards
beginPreparingDecks _ = error "Deck preparation should occur after the supply has been prepared"

addCardToDeck :: CandidateId -> Card -> GameState -> GameState
addCardToDeck pid card (PreparingDecks ps cards)
  | all ((/=) pid . PlayerPreparingStartingDeck.playerId) ps = error "Invalid deck preparation: player not in game"
  | otherwise = PreparingDecks (alterStartingDeck (card :) pid ps) cards
addCardToDeck _ _ _ = error "A card may only be added to a deck during game preparation"

beginDrawingInitialHands :: GameState -> GameState
beginDrawingInitialHands (PreparingDecks ps cards) =
  DrawingInitialHands (PlayerDrawingInitialHand.fromPlayerPreparingStartingDeck <$> ps) cards
beginDrawingInitialHands _ = error "Drawing initial hands must occur after decks have been prepared"

drawCard :: CandidateId -> Card -> GameState -> GameState
drawCard pid card (DrawingInitialHands ps cards)
  | all ((/=) pid . PlayerDrawingInitialHand.playerId) ps = error "Invalid card draw: player not in game"
  | (notElem card . PlayerDrawingInitialHand.deck <$> find ((==) pid . PlayerDrawingInitialHand.playerId) ps)
      == Just True = error "Invalid card draw: card not in deck of player"
  | otherwise =
    DrawingInitialHands
      (alterPlayerDrawingInitialHand
        (PlayerDrawingInitialHand.alterHand (card :) . PlayerDrawingInitialHand.alterDeck (delete card))
        pid
        ps)
      cards
drawCard _ _ _ = error "A card may only be drawn while players are drawing their initial hands"

beginPlay :: GameState -> GameState
beginPlay (DrawingInitialHands ps cards) = BuyPhase (BuyAllowance 1) (Player.fromPlayerDrawingInitialHand <$> ps) cards
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
gainCard _ _ _ = error "Cannot gain card when game is not in progress"

beginCleanUpPhase :: GameState -> GameState
beginCleanUpPhase (BuyPhase _ ps cards) = CleanUpPhase ps cards
beginCleanUpPhase _ = error "Clean up phase must follow buy phase"

alterWhere :: (a -> Bool) -> (a -> a) -> [a] -> [a]
alterWhere p f = fmap $ liftA3 bool id f p

alterElem :: Eq b => (a -> b) -> (a -> a) -> b -> [a] -> [a]
alterElem on f x = alterWhere ((==) x . on) f

alterStartingDeck ::
  ([Card] -> [Card])
  -> CandidateId
  -> [PlayerPreparingStartingDeck]
  -> [PlayerPreparingStartingDeck]
alterStartingDeck = alterElem PlayerPreparingStartingDeck.playerId . PlayerPreparingStartingDeck.alterDeck

alterPlayerDrawingInitialHand ::
  (PlayerDrawingInitialHand -> PlayerDrawingInitialHand)
  -> CandidateId
  -> [PlayerDrawingInitialHand]
  -> [PlayerDrawingInitialHand]
alterPlayerDrawingInitialHand = alterElem PlayerDrawingInitialHand.playerId

alterPlayer :: (Player -> Player) -> CandidateId -> [Player] -> [Player]
alterPlayer = alterElem Player.playerId

playerExists :: CandidateId -> [Player] -> Bool
playerExists pid = any ((==) pid . Player.playerId)
