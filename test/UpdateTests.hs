module UpdateTests where

import Update
import Command
import GameState
import Player

import ArbitraryInstances
import Test.Hspec
import Test.QuickCheck

updateTests :: SpecWith ()
updateTests = describe "update" $ do

  describe "add player to new game" $
    it "adds player" $ property $ \ps player ->
      length (players (update (AddPlayer player) (New ps))) == length ps + 1

  describe "players ready" $
    it "begins preparing supply" $ property $
      isPreparingSupply . update PlayersReady . New

  describe "place card in supply" $
    it "adds card" $ property $ \ps cards card ->
      length (supply (update (PlaceCardInSupply card) (PreparingSupply ps cards))) == length cards + 1

  describe "add card to deck of player" $
    it "adds card to deck of player" $ property $ \(PlayersAndSelectedPlayer ps pid) cards card ->
      any ((== pid) . playerId) (players (update (AddCardToDeck pid card) (PreparingDecks ps cards)))

isPreparingSupply :: GameState -> Bool
isPreparingSupply (PreparingSupply _ _) = True
isPreparingSupply _ = False
