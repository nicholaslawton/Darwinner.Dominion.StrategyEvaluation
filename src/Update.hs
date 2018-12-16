module Update (update, updateAction) where

import Command
import Game
import Player
import Card

update :: Command -> Game -> Game
update = updateState . updateAction

updateAction :: Command -> GameState -> GameState
updateAction (AddPlayer player) = addPlayer player
updateAction PlayersReady = beginPreparingSupply
updateAction (PlaceCardInSupply card) = placeCardInSupply card
updateAction Noop = const Prepared

updateState :: (GameState -> GameState) -> Game -> Game
updateState f game = game { Game.state = f (Game.state game) }

addPlayer :: Player -> GameState -> GameState
addPlayer player (New ps) = New $ player : ps
addPlayer _ _ = error "A player may not be added after preparation of the game has commenced"

beginPreparingSupply :: GameState -> GameState
beginPreparingSupply (New ps) = PreparingSupply ps []
beginPreparingSupply _ = error "Cannot prepare the supply of a game which has already begun"

placeCardInSupply :: Card -> GameState -> GameState
placeCardInSupply card (PreparingSupply ps supply) = PreparingSupply ps $ card : supply
placeCardInSupply _ _ = error "A card may only be placed in the supply during preparation"