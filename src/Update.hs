module Update (update) where

import Candidate
import Command
import GameState
import Player
import Card

import Data.List

update :: Command -> GameState -> GameState
update (AddPlayer pid) = addPlayer pid
update MarkPlayersReady = beginPreparingSupply
update (PlaceCardInSupply card) = placeCardInSupply card
update MarkSupplyPrepared = beginPreparingDecks
update (AddCardToDeck pid card) = addCardToDeck pid card
update MarkDecksPrepared = beginDrawingInitialHands
update (DrawCard pid card) = drawCard pid card
update MarkInitialHandsDrawn = const Prepared
update Noop = id

addPlayer :: CandidateId -> GameState -> GameState
addPlayer pid (New pids) = New $ pid : pids
addPlayer _ _ = error "A player may not be added after preparation of the game has commenced"

beginPreparingSupply :: GameState -> GameState
beginPreparingSupply (New pids) = PreparingSupply (Player.new <$> pids) []
beginPreparingSupply _ = error "Cannot prepare the supply of a game which has already begun"

placeCardInSupply :: Card -> GameState -> GameState
placeCardInSupply card (PreparingSupply ps cards) = PreparingSupply ps $ card : cards
placeCardInSupply _ _ = error "A card may only be placed in the supply during game preparation"

beginPreparingDecks :: GameState -> GameState
beginPreparingDecks (PreparingSupply ps cards) = PreparingDecks ps cards
beginPreparingDecks _ = error "Deck preparation should occur after the supply has been prepared"

addCardToDeck :: CandidateId -> Card -> GameState -> GameState
addCardToDeck pid card (PreparingDecks ps cards) = PreparingDecks (mapPlayer pid (mapDeck (card :)) ps) cards
addCardToDeck _ _ _ = error "A card may only be added to a deck during game preparation"

beginDrawingInitialHands :: GameState -> GameState
beginDrawingInitialHands (PreparingDecks ps cards) = DrawingInitialHands ps cards
beginDrawingInitialHands _ = error "Drawing initial hands must occur after decks have been prepared"

drawCard :: CandidateId -> Card -> GameState -> GameState
drawCard pid card (DrawingInitialHands ps cards) =
  DrawingInitialHands (mapPlayer pid (mapHand (card :) . mapDeck (delete card)) ps) cards
drawCard _ _ _ = error "A card may only be drawn while players are drawing their initial hands"

mapPlayer :: CandidateId -> (Player -> Player) -> [Player] -> [Player]
mapPlayer pid f = fmap (\p -> if playerId p == pid then f p else id p)
