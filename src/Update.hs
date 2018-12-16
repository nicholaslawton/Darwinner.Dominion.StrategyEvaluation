module Update (update) where

import Command
import GameState
import Player
import Card

update :: Command -> GameState -> GameState
update (AddPlayer player) = addPlayer player
update PlayersReady = beginPreparingSupply
update (PlaceCardInSupply card) = placeCardInSupply card
update Noop = const Prepared

addPlayer :: Player -> GameState -> GameState
addPlayer player (New ps) = New $ player : ps
addPlayer _ _ = error "A player may not be added after preparation of the game has commenced"

beginPreparingSupply :: GameState -> GameState
beginPreparingSupply (New ps) = PreparingSupply ps []
beginPreparingSupply _ = error "Cannot prepare the supply of a game which has already begun"

placeCardInSupply :: Card -> GameState -> GameState
placeCardInSupply card (PreparingSupply ps supply) = PreparingSupply ps $ card : supply
placeCardInSupply _ _ = error "A card may only be placed in the supply during preparation"