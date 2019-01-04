module Update (update) where

import Candidate
import Command
import GameState
import PlayerPreparingStartingDeck
import PlayerDrawingInitialHand
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
update MarkInitialHandsDrawn = const InProgress
update Noop = id

addPlayer :: CandidateId -> GameState -> GameState
addPlayer pid (New pids) = New $ pid : pids
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
addCardToDeck pid card (PreparingDecks ps cards) = PreparingDecks (alterStartingDeck (card :) pid ps) cards
addCardToDeck _ _ _ = error "A card may only be added to a deck during game preparation"

beginDrawingInitialHands :: GameState -> GameState
beginDrawingInitialHands (PreparingDecks ps cards) =
  DrawingInitialHands (PlayerDrawingInitialHand.fromPlayerPreparingStartingDeck <$> ps) cards
beginDrawingInitialHands _ = error "Drawing initial hands must occur after decks have been prepared"

drawCard :: CandidateId -> Card -> GameState -> GameState
drawCard pid card (DrawingInitialHands ps cards) =
  DrawingInitialHands
    (alterPlayerDrawingInitialHand
      (PlayerDrawingInitialHand.alterHand (card :) . PlayerDrawingInitialHand.alterDeck (delete card))
      pid
      ps)
    cards
drawCard _ _ _ = error "A card may only be drawn while players are drawing their initial hands"

alterWhere :: (a -> Bool) -> (a -> a) -> [a] -> [a]
alterWhere p f = fmap $ liftA3 bool id f p

alterElem :: Eq b => (a -> b) -> (a -> a) -> b -> [a] -> [a]
alterElem on f x = alterWhere ((==) x . on) f

alterStartingDeck ::
  ([Card] -> [Card])
  -> CandidateId
  -> [PlayerPreparingStartingDeck]
  -> [PlayerPreparingStartingDeck]
alterStartingDeck alteration =
  alterElem PlayerPreparingStartingDeck.playerId (PlayerPreparingStartingDeck.alterDeck alteration)

alterPlayerDrawingInitialHand ::
  (PlayerDrawingInitialHand -> PlayerDrawingInitialHand)
  -> CandidateId
  -> [PlayerDrawingInitialHand]
  -> [PlayerDrawingInitialHand]
alterPlayerDrawingInitialHand = alterElem PlayerDrawingInitialHand.playerId
